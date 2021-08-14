package breeze.macros

class LoopMacrosTest extends org.scalatest.funsuite.AnyFunSuite {

  // lifted from the spire tests.
  /*
  Copyright (c) 2011-2012 Erik Osheim, Tom Switzer

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
   */
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

  test("cforRange2") {
    var sum = 0
    cforRange2(1 until 5, 6 until 10) { (a, b) =>
      sum += a * b
    }

    assert(sum == (1 until 5).sum * (6 until 10).sum)

  }
}
