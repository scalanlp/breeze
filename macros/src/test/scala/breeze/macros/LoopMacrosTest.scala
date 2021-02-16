package breeze.macros

class LoopMacrosTest extends org.scalatest.funsuite.AnyFunSuite {

  test("cforRange(1 until 4)") {
    var t = 0
    cforRange(1 until 4) { x =>
      t += x
    }
    assert(t === 6)
  }

  test("cforRange(0 to 10 by 2)") {
    var t = 0
    cforRange(0 to 10 by 2) { x =>
      t += x
    }
    assert(t === 30)
  }

  test("cforRange(3 to 1 by -1)") {
    var t = 0
    cforRange(3 to 1 by -1) { x =>
      t += x
    }
    assert(t === 6)
  }

  test("cforRange(0 to 0 by -1)") {
    var t = 0
    cforRange(0 to 0 by -1) { x =>
      t += 1
    }
    assert(t === 1)
  }
  
}
