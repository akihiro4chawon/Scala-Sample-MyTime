
// 時刻を表現する型（interface）
abstract class TimePoint extends PartiallyOrdered[TimePoint] {
  // 有効な時刻なら Some[ValidTimePoint] そうでなければ None を返す
  def toOption: Option[ValidTimePoint]

  // 半順序の定義
  // ・自分も相手も有効な時刻ならば、Some(時刻としての比較値)
  // ・いずれかが無効な時刻ならば、None を返す
  def compareOption(that: TimePoint): Option[Int] =
    for {
      thisValidTime <- this
      thatValidTime <- that
    } yield (thisValidTime compare thatValidTime)

  // PartatiallyOrdered の抽象メソッドを上書き
  override def tryCompareTo[T <% PartiallyOrdered[T]](other: T): Option[Int] =
    Some(other) collect {case that: TimePoint => that} flatMap compareOption

  // 何分後の時刻
  def +(min: Int): TimePoint
}

// 有効な時刻を表現する class
// （あるいは、Haskell の maybe モナドで Just(timePoint) な状態）
case class ValidTimePoint (val inMinutes: Int) extends TimePoint with Ordered[ValidTimePoint] {
  // 有効な時刻であるので、Some(this)を返すように上書き
  override def toOption = Some(this)

  // 全順序の実装
  // ・有効な時刻同士は必ず比較できるので、内部表現の inMinutes で全順序比較
  override def compare(that: ValidTimePoint) =
    this.inMinutes compare that.inMinutes
  
  // 何時・何分
  def hour = inMinutes / 60
  def min = inMinutes % 60
  
  // 内部表現の通算分で示すのは格好悪いので上書き
  override def toString = "ValidTimePoint("+hour+", "+min+")"
  
  // 何分後の時刻  
  override def +(min: Int) = TimePoint(inMinutes + min)
  // 時刻差が何分あるか
  def -(that: ValidTimePoint): Int = this.inMinutes - that.inMinutes
}

// 無効な時刻を表現する singleton
// （あるいは、Haskell の maybe モナドで Nothing な状態）
// （ぶっちゃけると、時刻クラスの monadic zero です）
case object InvalidTimePoint extends TimePoint {
  // 無効な時刻であるので、None を返すように上書き
  override def toOption: Option[ValidTimePoint] = None
  override def +(min: Int) = this
}

// TimePoint の companion object
// （たとえば、ファクトリメソッドなどが定義される場所）
object TimePoint {
  // Factory method
  //   実引数が示す時刻が正当なら ValidTimePoint
  //   そうでなければ InvalidTimePoint を返す
  def apply(hour: Int, min: Int): TimePoint =
    monadicCreateTimePoint(mayBeInMinutes(hour, min))
  
  def apply(min: Int): TimePoint =
    monadicCreateTimePoint(mayBeInMinutes(min))
  
  private def monadicCreateTimePoint(mayBeMinutes: Option[Int]) =
    for {
      inMinutes <- mayBeMinutes
      timePoint = new ValidTimePoint(inMinutes)
    } yield timePoint // Option[ValidTimePoint] -> TimePoint に暗黙の変換

  // ユーティリティメソッド
  //   実引数の時刻が要件を満たす場合は Some(inMinutes)
  //   そうでなければ None を返す
  //   (前回の答案で示した実装は、そのままここに隠蔽されます)
  private def mayBeInMinutes(min: Int): Option[Int] = 
    Some(min) filter {_ <= 60 * 24}
    
  private def mayBeInMinutes(hour: Int, min: Int): Option[Int] =
    for {
      inMinutes <- mayBeInMinutes(60 * hour + min)
      if 0 to 24 contains hour
      if 0 to 60 contains min
    } yield inMinutes

  // Option モナドへの暗黙の変換
  //  本来なら、TimePoint extends Option[ValidTimePoint] としたいくらいだが、
  //  Option は sealed class なので、新たにサブクラスを追加できない
  //  そこで、暗黙の変換を許すことで実質的に Option[TimePoint]とみなせるようにする
  implicit def timePointToOption(tp: TimePoint): Option[ValidTimePoint] = tp.toOption
  implicit def optionToTimePoint(o: Option[ValidTimePoint]): TimePoint = o getOrElse InvalidTimePoint
}

object ValidTimePoint {
  implicit object ValidTimePointOrdering extends Ordering[ValidTimePoint] {
    override def compare(x: ValidTimePoint, y: ValidTimePoint) =
      x.inMinutes compare y.inMinutes
  }
}


// モナド脳
// ①明示的にunitを呼ぶ
//    Some(other) collect {case that: TimePoint => that} flatMap compareOption
// ②ライブラリ
//    PartialFunction.condOpt(v) { case that: TimePoint => that } flatMap compareOption 

// パターンマッチ脳
//    other match {
//      case that: TimePoint => compareOption(that)
//      case _ => None
//    }

// 条件分岐脳
//    if (other.isInstanceOf[TimePoint])
//      compareOption(other.asInstanceOf[TimePoint])
//    else
//      None
