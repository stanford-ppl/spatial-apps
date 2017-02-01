package spatial.api

import argon.core.{ArgonExceptions, Staging}
import spatial.SpatialExp

trait SpatialExceptions extends ArgonExceptions { self: SpatialExp =>
  // --- Compiler exceptions

  class EmptyReductionTreeLevelException(implicit ctx: SrcCtx) extends
  CompilerException(1000, c"Cannot create reduction tree for empty list.", {
    error(ctx, "Cannot create reduction tree for empty list.")
    error(ctx)
  })

  class UndefinedDimensionsError(s: Exp[_], d: Option[Exp[_]])(implicit ctx: SrcCtx) extends
  CompilerException(1001, c"Cannot find dimensions for symbol ${str(s)}.", {
    error(ctx, s"Cannot locate dimensions for symbol ${str(s)} used here.")
    if (d.isDefined) error(c"  Dimension: $d")
    error(ctx)
  })

  class UndefinedZeroException(s: Exp[_], tp: Staged[_])(implicit ctx: SrcCtx) extends
  CompilerException(1002, c"Unit Pipe Transformer could not create zero for type $tp for escaping value $s", {
    error(ctx, c"Unit Pipe Transformer could create zero for type $tp for escaping value $s")
  })

  class ExternalWriteError(mem: Exp[_], write: Exp[_])(implicit ctx: SrcCtx) extends
  CompilerException(1003, c"Found illegal write to memory $mem defined outside an inner controller", {
    error(ctx, c"Found illegal write to memory $mem defined outside an inner controller")
  })

  class UndefinedBankingException(tp: Staged[_])(implicit ctx: SrcCtx) extends
  CompilerException(1004, c"Don't know how to bank memory type $tp", {
    error(ctx, c"Don't know how to bank memory type $tp")
  })

  class UndefinedDispatchException(access: Exp[_], mem: Exp[_]) extends
  CompilerException(1005, c"Access $access had no dispatch information for memory $mem", {
    error(c"Access $access had no dispatch information for memory $mem")
  })

  class UndefinedPortsException(access: Exp[_], mem: Exp[_], idx: Option[Int]) extends
  CompilerException(1006, c"Access $access had no ports for memory $mem" + idx.map{i => c", index $i"}.getOrElse(""), {
    error(c"Access $access had no ports for memory $mem" + idx.map{i => c", index $i"}.getOrElse(""))
  })

  class NoCommonParentException(a: Ctrl, b: Ctrl) extends
  CompilerException(1007, c"Controllers $a and $b had no common parent while finding LCA with distance", {
    error(c"Controllers $a and $b had no common parent while finding LCA with distance")
  })

  class UndefinedChildException(parent: Ctrl, access: Access) extends
  CompilerException(1008, c"Parent $parent does not appear to contain $access while running childContaining", {
    error(c"Parent $parent does not appear to contain $access while running childContaining")
  })

  class AmbiguousMetaPipeException(mem: Exp[_], metapipes: Map[Ctrl, Seq[Access]]) extends
  CompilerException(1009, c"Ambiguous metapipes for readers/writers of $mem: ${metapipes.keySet}", {
    error(c"Ambiguous metapipes for readers/writers of $mem:")
    metapipes.foreach{case (pipe,accesses) => error(c"  $pipe: $accesses")}
  })

  class UndefinedPipeDistanceException(a: Ctrl, b: Ctrl) extends
  CompilerException(1010, c"Controllers $a and $b have an undefined pipe distance because they occur in parallel", {
    error(c"Controllers $a and $b have an undefined pipe distance because they occur in parallel")
  })



  // --- User exceptions
  class InvalidOnchipDimensionError(dim: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Memory defined here has invalid dimension $dim.")
    error("Only functions of constants and DSE parameters are allowed as dimensions of on-chip memories")
  })

  class InvalidParallelFactorError(par: Exp[_])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Invalid parallelization factor: ${str(par)}")
  })

  class DimensionMismatchError(mem: Exp[_], dims: Int, inds: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Invalid number of indices used to access $mem: Expected $dims, got $inds")
  })

  class SparseAddressDimensionError(dram: Exp[_], d: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Creation of multi-dimensional sparse DRAM tiles is currently unsupported.")
    error(c"Expected 1D SRAM tile, found ${d}D tile")
  })
  class SparseDataDimensionError(isLoad: Boolean, d: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"""Multi-dimensional ${if (isLoad) "gather" else "scatter"} is currently unsupported.""")
  })
  class UnsupportedStridedDRAMError(isLoad: Boolean)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"""Strided tile ${if (isLoad) "load" else "store"} is currently unsupported""")
  })

  class UnsupportedUnalignedTileStore(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Unaligned tile store is currently unsupported.")
  })

  class ControlInReductionError(ctx: SrcCtx) extends UserError(ctx, {
    error(ctx, c"Reduction functions cannot have inner control nodes")
  })

  class InvalidOffchipDimensionError(offchip: Exp[_], dim: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(offchip), c"Offchip memory defined here has invalid dimension $dim")
  })

  class ConcurrentReadersError(mem: Exp[_], a: Exp[_], b: Exp[_])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(mem)(ctx), c"${mem.tp} defined here has illegal concurrent readers: ")
    error(ctxOrHere(a)(ctx), c"  The first read occurs here")
    error(ctxOrHere(b)(ctx), c"  The second read occurs here")
  })

  class ConcurrentWritersError(mem: Exp[_], a: Exp[_], b: Exp[_])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(mem)(ctx), c"${mem.tp} defined here has illegal concurrent writers: ")
    error(ctxOrHere(a)(ctx), c"  The first write occurs here")
    error(ctxOrHere(b)(ctx), c"  The second write occurs here")
  })

  class PipelinedReadersError(mem: Exp[_], a: Exp[_], b: Exp[_])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(mem)(ctx), c"${mem.tp} defined here has illegal pipelined readers: ")
    error(ctxOrHere(a)(ctx), c"  The first read occurs here")
    error(ctxOrHere(b)(ctx), c"  The second read occurs here")
  })

  class PipelinedWritersError(mem: Exp[_], a: Exp[_], b: Exp[_])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(mem)(ctx), c"${mem.tp} defined here has illegal pipelined writers: ")
    error(ctxOrHere(a)(ctx), c"  The first write occurs here")
    error(ctxOrHere(b)(ctx), c"  The second write occurs here")
  })

  class MultipleReadersError(mem: Exp[_], readers: List[Exp[_]])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(mem)(ctx), c"${mem.tp} defined here has illegal multiple readers: ")
    readers.foreach{read => error(ctxOrHere(read)(ctx), c"  Read defined here") }
  })

  class MultipleWritersError(mem: Exp[_], writers: List[Exp[_]])(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(ctxOrHere(mem)(ctx), c"${mem.tp} defined here has illegal multiple writers: ")
    writers.foreach{write => error(ctxOrHere(write)(ctx), c"  Write defined here") }
  })

  class NoTopError(ctx: SrcCtx) extends ProgramError({
    error("An Accel block is required to specify the area of code to optimize for the FPGA.")
    error("No Accel block was specified for this program.")
  })

  class EmptyVectorException(ctx: SrcCtx) extends UserError(ctx, {
    error("Attempted to create an empty vector. Empty vectors are currently disallowed.")
  })

  class InvalidVectorApplyIndex(vector: Exp[_], index: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(u"Attempted to address vector $vector at invalid index $index.")
  })

  class InvalidVectorSlice(vector: Exp[_], start: Int, end: Int)(implicit ctx: SrcCtx) extends UserError(ctx, {
    error(u"Attempted to slice vector $vector from $start to $end, creating an empty vector")
    error("Creation of empty vectors is currently disallowed.")
  })
}