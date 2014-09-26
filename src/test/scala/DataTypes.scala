
object ADT {
  /**
    * Type Aliases
    */
  type UserID = String
  type Password = String
  type Permission = String
  case class User(id: String)

  sealed trait Limit
  case object Init extends Limit
  case object End  extends Limit

  /**
    * User Interaction ADT
    */
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]
  case object EndI extends Interact[End.type]

  sealed trait FileSystem[A]
  case object ReadLine extends FileSystem[Option[String]]
  case class PutLine(line: String) extends FileSystem[Int]
  case object Eof extends FileSystem[Unit]

  sealed trait Auth[A]
  case class Login(u: UserID, p: Password) extends Auth[Option[User]]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  sealed trait LogLevel
  case object ErrorLevel extends LogLevel
  case object WarnLevel extends LogLevel
  case object InfoLevel extends LogLevel
  case object DebugLevel extends LogLevel

  sealed trait LogA[A]
  case class Log(level: LogLevel, msg: String) extends LogA[Unit]

  sealed trait Storage[K, +V, A]
  case class Get[K, V](key: K) extends Storage[K, V, V]
  case class Put[K, V](key: K, value: V) extends Storage[K, V, Unit]
  case class Del[K, V](key: K) extends Storage[K, V, Unit]
}

object Interpreters {
  import ADT._
  import shapeless._
  import poly._

  // THE INTERPRETERS
  object Console extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine
      case Tell(msg) =>
        println(msg)
      case EndI => println("END"); End
    }
  }

  // object File extends (FileSystem ~> Id) {
  //   val l = Seq.fill(1000)("tata")
  //   var i = 0
  //   def apply[A](fs: FileSystem[A]) = fs match {
  //     case ReadLine =>
  //       if(i < l.size) { val r = Some(l(i)); i+=1; r }
  //       else None
  //     case PutLine(line) => println(line)
  //     case Eof => println("EOF")
  //   }
  // }

  import scalaz.{Free, Trampoline}

  // object File2 extends (FileSystem ~> Free.Trampoline) {
  //   val l = Seq.fill(100000)("tata")
  //   var i = 0
  //   def apply[A](fs: FileSystem[A]) = fs match {
  //     case ReadLine =>
  //       if(i < l.size) { val r = Some(l(i) + "_" + i); i+=1; Trampoline.done(r) }
  //       else Trampoline.done(None)
  //     case PutLine(line) =>
  //       //Trampoline.done(println(line))
  //       Trampoline.done(())
  //     case Eof =>
  //       Trampoline.done(println("EOF"))
  //   }
  // }

  // object File3 extends (FileSystem ~> TFree.Trampoline) {
  //   val l = Seq.fill(100000)("tata")
  //   var i = 0
  //   def apply[A](fs: FileSystem[A]) = fs match {
  //     case ReadLine =>
  //       if(i < l.size) { val r = Some(l(i) + "_" + i); i+=1; TFree.Trampoline.done(r) }
  //       else TFree.Trampoline.done(None)
  //     case PutLine(line) =>
  //       //Trampoline.done(println(line))
  //       TFree.Trampoline.done(())
  //     case Eof =>
  //       TFree.Trampoline.done(println("EOF"))
  //   }
  // }

  def fileInterpreter(n: Int) = new (FileSystem ~> Free.Trampoline) {
    val l = Seq.fill(n)("tata")
    var i = 0
    def apply[A](fs: FileSystem[A]) = fs match {
      case ReadLine =>
        if(i < l.size) { val r = Some(l(i) + "_" + i); i+=1; Trampoline.done(r) }
        else Trampoline.done(None)
      case PutLine(line) =>
        //Trampoline.done(println(line))
        Trampoline.done { println(i + " " + line); i }
      case Eof =>
        Trampoline.done(())
    }
  }

  def fileInterpreter2(n: Int) = new (FileSystem ~> strict.TFree.Trampoline) {
    //val l = Seq.fill(n)("tata")
    private var i = 0
    def apply[A](fs: FileSystem[A]) = fs match {
      case ReadLine =>
        if(i < n) { val r = Some("tata" + "_" + i); i+=1; strict.TFree.Trampoline.done(r) }
        else strict.TFree.Trampoline.done(None)
      case PutLine(line) =>
        strict.TFree.Trampoline.done { println(i + " " + line); i }
      case Eof =>
        strict.TFree.Trampoline.done(())
    }
  }

  def fileInterpreter2Lazy(n: Int) = new (FileSystem ~> `lazy`.TFree.Trampoline) {
    //val l = Seq.fill(n)("tata")
    private var i = 0
    def apply[A](fs: FileSystem[A]) = fs match {
      case ReadLine =>
        if(i < n) { val r = Some("tata" + "_" + i); i+=1; `lazy`.TFree.Trampoline.done(r) }
        else `lazy`.TFree.Trampoline.done(None)
      case PutLine(line) =>
        `lazy`.TFree.Trampoline.done { println(i + " " + line); i }
      case Eof =>
        `lazy`.TFree.Trampoline.done(())
    }
  }

  object Logger extends (LogA ~> Id) {
    def apply[A](a: LogA[A]) = a match {
      case Log(lvl, msg) =>
        println(s"$lvl $msg")
    }
  }

  object Logger2 extends (LogA ~> Free.Trampoline) {
    def apply[A](a: LogA[A]) = a match {
      case Log(lvl, msg) =>
        //Trampoline.done(println(s"$lvl $msg"))
        Trampoline.done(())
    }
  }

  object Logger3 extends (LogA ~> strict.TFree.Trampoline) {
    def apply[A](a: LogA[A]) = a match {
      case Log(lvl, msg) =>
        //Trampoline.done(println(s"$lvl $msg"))
        strict.TFree.Trampoline.done(())
    }
  }

  object Logger3Lazy extends (LogA ~> `lazy`.TFree.Trampoline) {
    def apply[A](a: LogA[A]) = a match {
      case Log(lvl, msg) =>
        //Trampoline.done(println(s"$lvl $msg"))
        `lazy`.TFree.Trampoline.done(())
    }
  }

  object TestAuth extends (Auth ~> Id) {
    def apply[A](a: Auth[A]) = a match {
      case Login(uid, pwd) =>
        if (uid == "john.snow" && pwd == "Ghost")
          Some(User("john.snow"))
        else None
      case HasPermission(u, _) =>
        u.id == "john.snow"
    }
  }

  val store = collection.mutable.Map[String, Int]()
  type StorageSI[A] = Storage[String, Int, A]

  object Store extends (StorageSI ~> Id) {
    def apply[A](a: Storage[String, Int, A]) = a match {
      case a:Get[String, Int] =>
        store(a.key)

      case Put(key, value) =>
        println(s"key:$key value:$value")
        store += (key -> value)
        ()
      case Del(key) =>
        store - key
        ()
    }
  }
}