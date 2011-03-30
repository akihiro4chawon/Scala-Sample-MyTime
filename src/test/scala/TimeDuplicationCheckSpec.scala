import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/*
前回の時間帯重複チェックについて、後日、お客様から仕様変更を言い渡されました。
仕様変更の内容は以下のようなものでした。

----------------------------------------------------------
時間帯は二つではなく、複数指定可能として欲しい。
また、重複していた場合、その時間帯を時間順で教えて欲しい。
----------------------------------------------------------

重複の定義、時間帯の形式*1は前回と同様のものとして以下のように変更して下さい。
・重複していた場合には、その重複している時間帯を必要分だけ返すようする
・返却する時間帯は開始時間が早いものから順に並べ替える
・同じ時間帯が２回以上重複している場合でも一つの時間帯とする（例４）
・重複していない場合には、なにも返さないようにする（例５）
*/

class TimeDuplicationCheckSuit extends FunSuite with ShouldMatchers {
  import Main.timeDuplicationCheck2
  
  //例1) 時間帯として二つ（午後0時から午後1時、午前10時から午後0時15分）が与えられた場合
  //入力：(12, 0, 13, 0), (10, 0, 12, 15)
  //出力：(12, 0, 12, 15)
  test("called with two periods that have one overlapping period") {
    val ret = timeDuplicationCheck2((12, 0, 13, 0), (10, 0, 12, 15))
    ret should be (Some("(12, 0, 12, 15)"))
  }
  
  //例2) 時間帯として三つ（午後4時から午後23時、午前5時から午前10時30分、午前9時から午後5時まで）が与えられた場合
  //入力：(16, 0, 23, 0), (9, 0, 17, 0), (5, 0, 10, 30)
  //出力：(9, 0, 10, 30), (16, 0, 17, 0)
  test("called with three periods that have two overlapping periods") {
    val ret = timeDuplicationCheck2((16, 0, 23, 0), (9, 0, 17, 0), (5, 0, 10, 30))
    ret should be (Some("(9, 0, 10, 30), (16, 0, 17, 0)"))
  }  

  //例3) 時間帯として六つ（午前9時から午後11時まで、午後1時から午後2時、午後3時から午後4時、午後5時から午後6時、午後7時から午後8時、午後9時から午後10時）が与えられた場合
  //入力：(12, 0, 23, 0), (13, 0, 14, 0), (15, 0, 16, 0), (17, 0, 18, 0), (19, 0, 20, 0), (21, 0, 22, 0)
  //出力：(13, 0, 14, 0), (15, 0, 16, 0), (17, 0, 18, 0), (19, 0, 20, 0), (21, 0, 22, 0)
  test("called with six periods that have five overlapping periods") {
    val ret = timeDuplicationCheck2((12, 0, 23, 0), (13, 0, 14, 0), (15, 0, 16, 0), (17, 0, 18, 0), (19, 0, 20, 0), (21, 0, 22, 0))
    ret should be (Some("(13, 0, 14, 0), (15, 0, 16, 0), (17, 0, 18, 0), (19, 0, 20, 0), (21, 0, 22, 0)"))
  }  
  
  //例4) 時間帯として何度も重複している場合
  //入力：(10, 0, 12, 0), (11, 0, 11, 30), (10, 30, 11, 15)
  //出力：(10, 30, 11, 30)
  test("check if multiple overlap periods are combined") {
    val ret = timeDuplicationCheck2((10, 0, 12, 0), (11, 0, 11, 30), (10, 30, 11, 15))
    ret should be (Some("(10, 30, 11, 30)"))
  }  
  
  //例5) 重複なし
  //入力：(9, 0, 17, 0), (19, 0, 21, 0)
  //出力：なし
  test("called with non overlapping time periods") {
    val ret = timeDuplicationCheck2((9, 0, 17, 0), (19, 0, 21, 0))
    ret should be (Some(""))
  }  

  test("called with illegal time period") {
    val ret = timeDuplicationCheck2((17, 0, 9, 0), (19, 0, 21, 0))
    ret should be (None)
  } 
  
  test("called with illegal time point") {
    val ret = timeDuplicationCheck2((17, 0, 9, 0), (19, 0, 21, 0))
    ret should be (None)
  } 
}
