import spatial.dsl._
import virtualized._


// TODO: need to have a function that can handle init-ting and flushing multiple cells in a list
trait RNNImps extends SpatialApp with CellImps {
  // TODO: need to figure out a way to add type cast for a class
  // override type T = FixPt[TRUE, _8, _8]

  class RNN[T  <: MetaAny[T] :Type:Num](xDRAM: DRAM3[T], cell: RNNCell[T], maxTime: Int = 1) {
    @virtualize
    def init() {
      cell.init()
    }

    @virtualize
    def flush() {
      cell.flush()
    }

    @virtualize
    def forward() {
      init()

      Sequential.Foreach(maxTime by 1) { timeStep =>
        cell.x load xDRAM(0::cell.batch_size, timeStep, 0::cell.feature_size)
        if (cell.isResidual)
          cell.forward(timeStep == (maxTime - 1))
        else
          cell.forward()
      }

      flush()
    }
  }


  class BiRNN[T  <: MetaAny[T] :Type:Num](xDRAM: DRAM3[T], fwCell: RNNCell[T], bwCell: RNNCell[T], maxTime: Int = 1) {
    @virtualize
    def init() {
      Parallel {
        fwCell.init()
        bwCell.init()
      }
    }


    @virtualize
    def flush() {
      Parallel {
        fwCell.flush()
        bwCell.flush()
      }
    }

    @virtualize
    def forward() {
      init()

      Sequential.Foreach(maxTime by 1) { timeStep =>
        Parallel {
          // fw cell
          Pipe {
            fwCell.x load xDRAM(0::fwCell.batch_size, timeStep, 0::fwCell.feature_size)
            fwCell.forward()
          }

          // bw cell
          Pipe {
            bwCell.x load xDRAM(0::bwCell.batch_size, maxTime - timeStep - 1, 0::bwCell.feature_size)
            bwCell.forward()
          }
        }
      }

      flush()
    }
  }
}
