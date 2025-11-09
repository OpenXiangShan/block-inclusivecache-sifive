/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkB(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = IO(new Bundle {
    val req = Decoupled(new FullRequest(params))
    val b = Flipped(Decoupled(new TLBundleB(params.outer.bundle)))
  })
  io.req := DontCare

  when (io.req.fire) {
    DebugPrint(params, "sinkB req ")
    io.req.bits.dump
  }

  /*
  when (io.b.fire) {
    DebugPrint(params, "inner probe ")
    io.b.bits.dump
  }
  */

  val b = params.micro.innerBuf.b(io.b)

  val (tag, set, offset) = params.parseAddress(b.bits.address)

  b.ready := io.req.ready
  io.req.valid := b.valid
  params.ccover(b.valid && !b.ready, "SINKB_STALL", "Backpressure when accepting a probe message")

  io.req.bits.prio   := 2.U(3.W).asBools
  io.req.bits.control:= false.B
  io.req.bits.opcode := b.bits.opcode
  io.req.bits.param  := b.bits.param
  io.req.bits.size   := b.bits.size
  io.req.bits.source := b.bits.source
  io.req.bits.offset := offset
  io.req.bits.set    := set
  io.req.bits.tag    := tag
}
