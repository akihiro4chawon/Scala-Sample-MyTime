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

  // 仕様変更で絶賛大炎上中！！！（＞＜）
  def timeDuplicationCheck2(durations: Duration*) = {
    def toHourMin(min: Int) = Seq(min / 60, min % 60)
    def separate(l: List[Int]) = {
      val sep = for {
        (a, b) <- (-10 +: l) zip (l :+ -10) if b - a != 1
        x <- Seq(a, b) if x > 0
      } yield x
      (sep grouped 2).toSeq flatMap {case Seq(m1, m2) => Seq(m1, m2 + 1)} flatMap toHourMin grouped 4 map {_.mkString("(", ", ", ")")}
    }
    for {
      minSet <- (Option(Seq[Int]()) /: (durations map durationInMinutes)) ((xsOpt, xOpt) => for (xs <- xsOpt; x <- xOpt) yield x ++ xs)
    } yield separate((minSet groupBy identity collect {case (m, ml) if ml.size > 1 => m}).toList.sorted).mkString("(", ", ", ")")
  }

  println(timeDuplicationCheck2( (10, 0, 12, 0), (11, 0, 11, 30), (10, 30, 11, 15) )) // => Sone(10, 30, 11, 30)
  println(timeDuplicationCheck2( (9, 0, 17, 0), (19, 0, 21, 0) )) // => Some(())

  println(timeDuplicationCheck2( (19, 0, 17, 0), (19, 0, 21, 0) )) // => None // 入力時刻がおかしい場合
}

