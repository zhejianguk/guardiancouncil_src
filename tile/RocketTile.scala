// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{DCacheLogicalTreeNode, LogicalModuleTree, RocketLogicalTreeNode, UTLBLogicalTreeNode}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.TileCrossingParamsLike
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkParameters}
//===== GuardianCouncil Function: Start ====//
import freechips.rocketchip.guardiancouncil._
//===== GuardianCouncil Function: End   ====//


case class RocketTileParams(
    core: RocketCoreParams = RocketCoreParams(),
    icache: Option[ICacheParams] = Some(ICacheParams()),
    dcache: Option[DCacheParams] = Some(DCacheParams()),
    btb: Option[BTBParams] = Some(BTBParams()),
    dataScratchpadBytes: Int = 0,
    name: Option[String] = Some("tile"),
    hartId: Int = 0,
    beuAddr: Option[BigInt] = None,
    blockerCtrlAddr: Option[BigInt] = None,
    clockSinkParams: ClockSinkParameters = ClockSinkParameters(),
    boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
    ) extends InstantiableTileParams[RocketTile] {
  require(icache.isDefined)
  require(dcache.isDefined)
  def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): RocketTile = {
    new RocketTile(this, crossing, lookup)
  }
}

class RocketTile private(
      val rocketParams: RocketTileParams,
      crossing: ClockCrossingType,
      lookup: LookupByHartIdImpl,
      q: Parameters)
    extends BaseTile(rocketParams, crossing, lookup, q)
    with SinksExternalInterrupts
    with SourcesExternalNotifications
    with HasLazyRoCC  // implies CanHaveSharedFPU with CanHavePTW with HasHellaCache
    with HasHellaCache
    with HasICacheFrontend
{
  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: RocketTileParams, crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  override val logicalTreeNode = new RocketLogicalTreeNode(this, p(XLen), pgLevels)

  val dtim_adapter = tileParams.dcache.flatMap { d => d.scratch.map { s =>
    LazyModule(new ScratchpadSlavePort(AddressSet.misaligned(s, d.dataScratchpadBytes), lazyCoreParamsView.coreDataBytes, tileParams.core.useAtomics && !tileParams.core.useAtomicsOnlyForIO))
  }}
  dtim_adapter.foreach(lm => connectTLSlave(lm.node, lm.node.portParams.head.beatBytes))

  val bus_error_unit = rocketParams.beuAddr map { a =>
    val beu = LazyModule(new BusErrorUnit(new L1BusErrors, BusErrorUnitParams(a), logicalTreeNode))
    intOutwardNode := beu.intNode
    connectTLSlave(beu.node, xBytes)
    beu
  }

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode
  DisableMonitors { implicit p => tlSlaveXbar.node :*= slaveNode }

  nDCachePorts += 1 /*core */ + (dtim_adapter.isDefined).toInt

  val dtimProperty = dtim_adapter.map(d => Map(
    "sifive,dtim" -> d.device.asProperty)).getOrElse(Nil)

  val itimProperty = frontend.icache.itimProperty.toSeq.flatMap(p => Map("sifive,itim" -> p))

  val beuProperty = bus_error_unit.map(d => Map(
          "sifive,buserror" -> d.device.asProperty)).getOrElse(Nil)

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("sifive,rocket0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ cpuProperties ++ nextLevelCacheProperty
                  ++ tileProperties ++ dtimProperty ++ itimProperty ++ beuProperty)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(staticIdForMetadataUseOnly))
  }

  override lazy val module = new RocketTileModuleImp(this)

  override def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!rocketParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
    case _ => TLBuffer(BufferParams.none)
  }

  override def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!rocketParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
    case _ => TLBuffer(BufferParams.none)
  }

  val dCacheLogicalTreeNode = new DCacheLogicalTreeNode(dcache, dtim_adapter.map(_.device), rocketParams.dcache.get)
  LogicalModuleTree.add(logicalTreeNode, iCacheLogicalTreeNode)
  LogicalModuleTree.add(logicalTreeNode, dCacheLogicalTreeNode)

  if (rocketParams.core.useVM) {
    val utlbLogicalTreeNode = new UTLBLogicalTreeNode(rocketParams.core, utlbOMSRAMs)
    LogicalModuleTree.add(logicalTreeNode, utlbLogicalTreeNode)
  }
}

class RocketTileModuleImp(outer: RocketTile) extends BaseTileModuleImp(outer)
    with HasFpuOpt
    with HasLazyRoCCModule
    with HasICacheFrontendModule {
  Annotated.params(this, outer.rocketParams)

  val core = Module(new Rocket(outer)(outer.p))
  val ght_bridge = Module(new GH_Bridge(GH_BridgeParams(1)))
  val ghe_bridge = Module(new GH_Bridge(GH_BridgeParams(5)))
  val ght_cfg_bridge = Module(new GH_Bridge(GH_BridgeParams(32)))
  val ght_cfg_v_bridge = Module(new GH_Bridge(GH_BridgeParams(1)))

  //===== GuardianCouncil Function: Start ====//
  if (outer.tileParams.hartId == 0) {
    println("#### Jessica #### Generating GHT for the big core, HartID: ", outer.rocketParams.hartId, "...!!!")
    val ght = Module(new GHT(GHTParams(vaddrBitsExtended, p(XLen), 32, 32, 16, 128, 1, false)))    // revisit: set 32 as the total number of checkers.
                                                                                                   // revisit: total types of insts is 32
                                                                                                   // revisit: total number of SEs is 16 
                                                                                                   // revisit: packet size: 128 bits
    ght.io.ght_pcaddr_in := core.io.pc
    ght.io.ght_inst_in := core.io.inst
    ght.io.ght_alu_in := core.io.alu_2cycle_delay
    ght.io.ght_mask_in := ght_bridge.io.out
    ght.io.ght_cfg_in := ght_cfg_bridge.io.out
    ght.io.ght_cfg_valid := ght_cfg_v_bridge.io.out
    outer.ght_packet_out_SRNode.bundle := ght.io.ght_packet_out
    outer.ght_packet_dest_SRNode.bundle := ght.io.ght_packet_dest
    core.io.clk_enable_gh := ~(outer.bigcore_hang_in_SKNode.bundle) 
    outer.ghe_event_out_SRNode.bundle := ghe_bridge.io.out
    ght.io.core_na := outer.sch_na_inSKNode.bundle
    ght.io.new_commit := core.io.new_commit
    outer.ghm_agg_core_id_out_SRNode.bundle := ght.io.ghm_agg_core_id
  } else
  { // Other cores:
    // For other cores: no GHT is required, and hence tied-off
    core.io.clk_enable_gh := 1.U // the core is never gated
    outer.ght_packet_out_SRNode.bundle := 0.U
    outer.ght_packet_dest_SRNode.bundle := 0.U
    outer.ghe_event_out_SRNode.bundle := ghe_bridge.io.out
  }
    
  //===== GuardianCouncil Function: End ====//



  // Report unrecoverable error conditions; for now the only cause is cache ECC errors
  outer.reportHalt(List(outer.dcache.module.io.errors))

  // Report when the tile has ceased to retire instructions; for now the only cause is clock gating
  outer.reportCease(outer.rocketParams.core.clockGate.option(
    !outer.dcache.module.io.cpu.clock_enabled &&
    !outer.frontend.module.io.cpu.clock_enabled &&
    !ptw.io.dpath.clock_enabled &&
    core.io.cease))

  outer.reportWFI(Some(core.io.wfi))

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  outer.bus_error_unit.foreach { beu =>
    core.io.interrupts.buserror.get := beu.module.io.interrupt
    beu.module.io.errors.dcache := outer.dcache.module.io.errors
    beu.module.io.errors.icache := outer.frontend.module.io.errors
  }

  core.io.interrupts.nmi.foreach { nmi => nmi := outer.nmiSinkNode.bundle }

  // Pass through various external constants and reports that were bundle-bridged into the tile
  outer.traceSourceNode.bundle <> core.io.trace
  core.io.traceStall := outer.traceAuxSinkNode.bundle.stall
  outer.bpwatchSourceNode.bundle <> core.io.bpwatch
  core.io.hartid := outer.hartIdSinkNode.bundle
  require(core.io.hartid.getWidth >= outer.hartIdSinkNode.bundle.getWidth,
    s"core hartid wire (${core.io.hartid.getWidth}b) truncates external hartid wire (${outer.hartIdSinkNode.bundle.getWidth}b)")

  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.imem
  dcachePorts += core.io.dmem // TODO outer.dcachePorts += () => module.core.io.dmem ??
  fpuOpt foreach { fpu => core.io.fpu <> fpu.io }
  core.io.ptw <> ptw.io.dpath

  // Connect the coprocessor interfaces
  if (outer.roccs.size > 0) {
    cmdRouter.get.io.in <> core.io.rocc.cmd
    outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
    core.io.rocc.resp <> respArb.get.io.out
    core.io.rocc.busy <> (cmdRouter.get.io.busy || outer.roccs.map(_.module.io.busy).reduce(_ || _))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_ || _)
    //===== GuardianCouncil Function: Start ====//
    cmdRouter.get.io.ghe_packet_in := ((outer.ghe_packet_in_SKNode.bundle) | (outer.agg_packet_in_SKNode.bundle))
    cmdRouter.get.io.ghe_status_in := outer.ghe_status_in_SKNode.bundle
    ghe_bridge.io.in := cmdRouter.get.io.ghe_event_out
    ght_bridge.io.in := cmdRouter.get.io.ght_mask_out
    ght_cfg_bridge.io.in := cmdRouter.get.io.ght_cfg_out
    ght_cfg_v_bridge.io.in := cmdRouter.get.io.ght_cfg_valid
    outer.ght_status_out_SRNode.bundle := cmdRouter.get.io.ght_status_out

    // agg
    outer.agg_packet_out_SRNode.bundle := cmdRouter.get.io.agg_packet_out
    cmdRouter.get.io.agg_buffer_full := outer.agg_buffer_full_in_SKNode.bundle
    outer.agg_core_status_SRNode.bundle := cmdRouter.get.io.agg_core_status_out
    outer.ght_sch_na_out_SRNode.bundle := cmdRouter.get.io.ght_sch_na_out
    cmdRouter.get.io.ght_sch_refresh := outer.ghe_sch_refresh_in_SKNode.bundle
    cmdRouter.get.io.ght_buffer_status := 0.U
    // For big_core GHT
    cmdRouter.get.io.bigcore_comp := outer.bigcore_comp_in_SKNode.bundle
    outer.ght_sch_dorefresh_SRNode.bundle := cmdRouter.get.io.ght_sch_dorefresh_out                           
    //===== GuardianCouncil Function: End   ====//
  }

  // Rocket has higher priority to DTIM than other TileLink clients
  outer.dtim_adapter.foreach { lm => dcachePorts += lm.module.io.dmem }

  // TODO eliminate this redundancy
  val h = dcachePorts.size
  val c = core.dcacheArbPorts
  val o = outer.nDCachePorts
  require(h == c, s"port list size was $h, core expected $c")
  require(h == o, s"port list size was $h, outer counted $o")
  // TODO figure out how to move the below into their respective mix-ins
  dcacheArb.io.requestor <> dcachePorts
  ptw.io.requestor <> ptwPorts
}

trait HasFpuOpt { this: RocketTileModuleImp =>
  val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
}
