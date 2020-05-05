package optics.profunctor.mono

import optics.profunctor.poly.Forget

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State, Setup}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
object GetterBenchmark {

  @State(Scope.Thread)
  class TestState {

    // Stick to 2 parameters
    final case class Foo(s1: String, s2: String)

    final case class Getter[A, B](get: A => B)

    var foo: Foo = _
    var compiled: Foo => String = _
    var getter: Getter[Foo, String] = _

    @Setup
    def setup(): Unit = {
      foo = Foo("test", "bar")
      compiled = Lens[Foo, String](_.s2, foo => s => foo.copy(s2 = s)).get
      getter = Getter[Foo, String](_.s2)
    }
  }

}

class GetterBenchmark {

  @Benchmark
  def getArity2(state: GetterBenchmark.TestState): String = {
    state.compiled(state.foo)
  }

  @Benchmark
  def getArity2Comparison(state: GetterBenchmark.TestState): String = {
    state.getter.get(state.foo)
  }

}
