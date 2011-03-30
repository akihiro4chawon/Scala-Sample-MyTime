// お題：時間帯重複チェック(応用編)（リファクタリング前）
// <http://d.hatena.ne.jp/fumokmm/20110329/1301403400>
// お客さんから仕様変更を言い渡されて、さあ大変。
// まずはスパゲチーソースで即応だ！ by @akihiro4chawon

object Main extends Application {
  type IntTuple4 = (Int, Int, Int, Int)

  def timeDuplicationCheck2(durations: IntTuple4*) = {
    def format(p: ValidTimePeriod) = 
      (Seq(p.begin, p.end) flatMap {t => Seq(t.hour, t.min)}).mkString("(", ", ", ")") 
      
    val periods = durations map (TimePeriod.fromHoursMinutes _).tupled
    TimePeriod.overlappingPeriod(periods :_*) map {_ map format mkString ", "} 
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
