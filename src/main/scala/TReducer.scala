// import scalaz.{Monoid, Foldable}

// sealed abstract class TReducer[C[_, _], M] {

//   def unit[A, B](c: C[A, B]): M

//   /** Faster `append(m, unit(c))`. */
//   def snoc[A, B](m: M, c: C[A, B]): M

//   /** Faster `append(unit(c), m)`. */
//   def cons[A, B](c: C[A, B], m: M): M

//   def zero: M

//   def append(a1: M, a2: => M): M

//   /** Distribute `C`s to `M` and `N`. */
//   def compose[N](r: TReducer[C, N]): TReducer[C, (M, N)] = {
//     new TReducer[C, (M, N)] {

//       import scalaz.std.tuple._

//       override def unit[A, B](x: C[A, B]) = (TReducer.this.unit(x), r.unit(x))

//       override def snoc[A, B](p: (M, N), x: C[A, B]) = (TReducer.this.snoc(p._1, x), r.snoc(p._2, x))

//       override def cons[A, B](x: C[A, B], p: (M, N)) = (TReducer.this.cons(x, p._1), r.cons(x, p._2))

//       def zero = 
//     }
//   }
// }


// sealed abstract class UnitTReducer[C[_, _], M] extends TReducer[C, M] {
//   implicit def monoid: Monoid[M]
//   def unit[A, B](c: C[A, B]): M

//   def snoc[A, B](m: M, c: C[A, B]): M = monoid.append(m, unit(c))

//   def cons[A, B](c: C[A, B], m: M): M = monoid.append(unit(c), m)
// }

// object UnitTReducer {
//   /** Minimal `Reducer` derived from a monoid and `unit`. */
//   def apply[C[_, _], M](u: C ~~> M)(implicit mm: Monoid[M]): TReducer[C, M] = new UnitTReducer[C, M] {
//     val monoid = mm
//     def unit[A, B](c: C[A, B]) = u(c)
//   }
// }

// object TReducer extends TReducerFunctions {
//   /** Reducer derived from `unit`, `cons`, and `snoc`.  Permits more
//     * sharing than `UnitReducer.apply`.
//     */
//   def apply[C[_, _], M](u: C ~~> M, cs: C ~~> (M => M), sc: M => (C ~~> M))(implicit mm: Monoid[M]): TReducer[C, M] =
//     treducer(u, cs, sc)
// }

// sealed abstract class ReducerInstances { self: ReducerFunctions =>
//   import shapeless.{HList, ::, HNil}

//   /** Collect `C`s into a list, in order. */
//   implicit def ListReducer[C[_, _]]: Reducer[C, HList[C]] = {
//     import std.list._
//     object f1 extends C ~~> HList
//     unitConsReducer(_ :: HNil, c => c :: _)
//   }

// }

// trait TReducerFunctions {

//   /** Alias for [[TReducer]]`.apply`. */
//   def treducer[C[_, _], M](u: C ~~> M, cs: C ~~> (M => M), sc: M => (C ~~> M))(implicit mm: Monoid[M]): TReducer[C, M] =
//     new TReducer[C, M] {
//       val monoid = mm

//       def unit[A, B](c: C[A, B]) = u(c)

//       def snoc[A, B](m: M, c: C[A, B]): M = sc(m)(c)

//       def cons[A, B](c: C[A, B], m: M): M = cs(c)(m)
//     }

//   def foldReduce[F[_], C[_, _], A, B, M](a: F[C[A, B]])(implicit f: Foldable[F], r: TReducer[C, M]): M =
//     f.foldMap(a)(r.unit(_))(r.monoid)

//   /** Alias for [[scalaz.UnitReducer]]`.apply`. */
//   def unitTReducer[C[_, _], M](u: C ~~> M)(implicit mm: Monoid[M]): TReducer[C, M] =
//     new UnitTReducer[C, M] {
//       val monoid = mm
//       def unit[A, B](c: C[A, B]) = u(c)
//     }

//   def unitConsTReducer[C[_, _], M](u: C ~~> M, cs: C ~~> (M => M))(implicit mm: Monoid[M]): TReducer[C, M] = new TReducer[C, M] {
//     val monoid = mm

//     def unit[A, B](c: C[A, B]) = u(c)

//     def snoc[A, B](m: M, c: C[A, B]): M = mm.append(m, u(c))

//     def cons[A, B](c: C[A, B], m: M): M = cs(c)(m)
//   }

//   /** The reducer derived from any monoid.  Not implicit because it is
//     * suboptimal for most reducer applications.
//     */
//   // def identityReducer[M](implicit mm: Monoid[M]): Reducer[M, M] = unitReducer(x => x)
// }