
package freechips.rocketchip.guardiancouncil

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._


class WithGHE extends Config((site, here, up) => {
  case BuildRoCC => List(
    (p: Parameters) => {
        val ghe_loopback = LazyModule(new GHE(OpcodeSet.custom1)(p))
        ghe_loopback
    })
})