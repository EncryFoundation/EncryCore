import scala.reflect.ClassTag

trait A
case class B() extends A
case class C() extends A

val a = List(C(), C(), C(), B(), B())

def typedFilter[T <: A : ClassTag](in: List[A]): List[T] =
  in.collect{ case t: T =>
    println(s"t is: ${t}.Is type of t is T. ${t.isInstanceOf[T]}")
    t
  }

println(typedFilter[B](a))