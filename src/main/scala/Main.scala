// お題：時間帯重複チェック(応用編)（リファクタリング前）
// <http://d.hatena.ne.jp/fumokmm/20110329/1301403400>
// お客さんから仕様変更を言い渡されて、さあ大変。
// まずはスパゲチーソースで即応だ！ by @akihiro4chawon

object Main extends Application {
  type Duration = (Int, Int, Int, Int)

  def timePointInMinutes(hour: Int, min: Int) =
    for {
      inMinutes <- Some(60 * hour + min)
      if inMinutes <= 60 * 24
      if 0 to 24 contains hour
      if 0 to 60 contains min
    } yield inMinutes

  def durationInMinutes(d: Duration) = {
    val (h1, m1, h2, m2) = d
    for {
      t1 <- timePointInMinutes(h1, m1)
      t2 <- timePointInMinutes(h2, m2)
      if (t1 <= t2)
    } yield t1 until t2
  }

  def timeDuplicationCheck2(durations: Duration*) = {
    // List[Option[A]] -> Option[List[A]] への変換
    // (元のリストに一つでも None が含まれれば None； 全て Some の場合にのみ Some(List)
    // (scalaz.MA.sequence (F[G[A] => G[F[A]]) の F/G を List/Option 限定したもの)
    def sequence[A](so: Seq[Option[A]]): Option[List[A]] =
      so.foldLeft(Option(List[A]())) {(os, o) => for (s <- os; e <- o) yield e :: s}
    
    def toHourMin(min: Int) = Seq(min / 60, min % 60)
    
    def combineIntoPeriods(dupMins: Seq[Int]) = {
      // 区切り(増分が1以上)を列挙
      def delimit(l: Seq[Int]) = for {
        (a, b) <- (-10 +: l) zip (l :+ -10) if b - a != 1
        x <- Seq(a + 1, b) if x > 0
      } yield x
      delimit(dupMins) flatMap toHourMin grouped 4 map {_.mkString("(", ", ", ")")}
    }

    // 重複している要素のみを返す
    def filterDuplicatedMinutes(minSet: Seq[Int]) =
      (minSet groupBy identity collect {case (e, l) if l.size > 1 => e}).toSeq.sorted
    
    for (minRange <- sequence(durations map durationInMinutes))
      yield combineIntoPeriods(filterDuplicatedMinutes(minRange.flatten)) mkString ", "
  }

  println(timeDuplicationCheck2( (10, 0, 12, 0), (11, 0, 11, 30), (10, 30, 11, 15) )) // => Some(10, 30, 11, 30)
  println(timeDuplicationCheck2( (9, 0, 17, 0), (19, 0, 21, 0) )) // => Some(())

  println(timeDuplicationCheck2( (19, 0, 17, 0), (19, 0, 21, 0) )) // => None // 入力時刻がおかしい場合
}


    // 重複している要素のみを返す
//    def filterDuplicatedMinutes(minSet: Seq[Int]) = {
//      val tbl = Array.fill(24 * 60)(0)
//      minSet foreach {tbl(_) += 1}
//      0 until 24 * 60 filter {tbl(_) > 1}
//    }
