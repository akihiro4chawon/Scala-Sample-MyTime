
abstract class TimePeriod {
  // 有効な時刻なら Some[ValidTimePeriod] そうでなければ None を返す
  def toOption: Option[ValidTimePeriod]
}

case class ValidTimePeriod(
    val begin: ValidTimePoint,
    val end: ValidTimePoint
) extends TimePeriod with IndexedSeq[ValidTimePoint] {
  
  def toOption = Some(this)
  
  override def apply(idx: Int) =
    if (isDefinedAt(idx)) (begin + idx).get
    else throw new IndexOutOfBoundsException(idx.toString)
  
  override def length = end - begin
}

case object InvalidTimePeriod extends TimePeriod {
  def toOption = None
}

object TimePeriod {
  // factory methods
  def apply(begin: TimePoint, end: TimePoint) = fromTimePoints(begin, end)
  def apply(h1: Int, m1: Int, h2: Int, m2: Int) = fromHoursMinutes(h1, m1, h2, m2)
    
  def fromTimePoints(begin: TimePoint, end: TimePoint): TimePeriod =
    for { b <- begin; e <- end; if b <= e } yield ValidTimePeriod(b, e)
    
  def fromHoursMinutes(h1: Int, m1: Int, h2: Int, m2: Int): TimePeriod =
    TimePeriod(TimePoint(h1, m1), TimePoint(h2, m2))

  implicit def timePeriodToOption(tp: TimePeriod): Option[ValidTimePeriod] = tp.toOption
  implicit def optionToTimePeriod(o: Option[ValidTimePeriod]): TimePeriod = o getOrElse InvalidTimePeriod

  def overlappingPeriod(periods: TimePeriod*) = {
    // scalaz.MA.sequence (F[G[A] => G[F[A]]) のようなもの
    def sequence[A, G <% Option[A]](so: Seq[G]): Option[List[A]] =
      so.foldLeft(Option(List[A]())) {(os, o) => for (s <- os; e <- o) yield e :: s}
    
    def combineIntoPeriods(dupMins: Seq[ValidTimePoint]) = {
      // 区切り(増分が1以上)を列挙
      val guard = InvalidTimePoint
      def delimit(l: Seq[ValidTimePoint]) = for {
        (a, b) <- (guard +: l) zip (l :+ guard) if a + 1 != b
        timePoint <- Seq(a + 1, b)
        validTimePoint <- timePoint
      } yield validTimePoint
      delimit(dupMins) grouped 2 map {case Seq(b, e) => ValidTimePeriod(b, e)}
    }

    // 重複している要素のみを返す
    def filterDuplicatedMinutes(minSet: Seq[ValidTimePoint]) =
      (minSet groupBy identity collect {case (e, l) if l.size > 1 => e}).toSeq.sorted
    
    for (validPeriods <- sequence(periods))
      yield combineIntoPeriods(filterDuplicatedMinutes(validPeriods.flatten))
  }  
}

//  import PartialFunction.cond
//  def hasIntersectionOption(timePeriodSeq: TimePeriod*): Option[Boolean] = {
//    timePeriodSeq sortBy {_.begin} grouped 2 forall { cond(_) {case Seq(Some(p1), Some(p2)) => p1.end <= p2.begin} }
//  }

