
// sealed abstract class TFinger[V, R[_, _], A, B]

// case class TOne[V, R[_, _], A, B](v: V, a1: R[A, B])
//                                  (implicit r: TReducer[R, V])
//                                  extends TFinger[V, R, A, B]

// case class TTwo[V, R[_, _], A, B, C](v: V, a1: R[A, B], a2: R[B, C])
//                                     (implicit r: TReducer[R, V])
//                                     extends TFinger[V, R, A, C]

// case class TThree[V, R[_, _], A, B, C, D](v: V, a1: R[A, B], a2: R[B, C], a3: R[C, D])
//                                     (implicit r: TReducer[R, V])
//                                     extends TFinger[V, R, A, D]

// case class TFour[V, R[_, _], A, B, C, D, E](v: V, a1: R[A, B], a2: R[B, C], a3: R[C, D], a4: R[D, E])
//                                     (implicit r: TReducer[R, V])
//                                     extends TFinger[V, R, A, E]

// sealed abstract class TNode[V, R[_, _], A, B](implicit r: TReducer[R, V]) {

//   def fold[X, C, D](
//     two: (V, => R[A, B], => R[B, C]) => X,
//     three: (V, => R[A, B], => R[B, C], => R[C, D]) => X): X

// }


// sealed abstract class TFingerTree[V, R[_, _], A, B](implicit measurer: TReducer[R, V]) {
//   def measure: V = this.unit[V]

//   def fold[X](
//     empty: V => X,
//     single: (V, R[A, B]) => X,
//     deep: (V, Finger[V, R[A, B]], => FingerTree[V, Node[V, R[B, C]]], Finger[V, R[C, D]]) => X): X

// }