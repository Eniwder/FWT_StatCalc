object Main extends App {

  case class Stat(hp: Int = 0, mp: Int = 0, atk: Int = 0, defence: Long = 0, critRate: Int = 0, critDamage: Double = 0,
    dodge: Int = 0, hit: Int = 0, counterDamage: Int = 0, mastery: Int = 0, counterRate: Int = 0) {

    def +(that: Stat): Stat = {
      Stat(hp + that.hp, mp + that.mp, atk + that.atk, defence + that.defence,
        critRate + that.critRate, critDamage + that.critDamage, dodge + that.dodge,
        hit + that.hit, counterDamage + that.counterDamage, mastery + that.mastery, counterRate + that.counterRate)
    }

    override def toString: String = {
      val sb = StringBuilder.newBuilder
      if (hp != 0) sb ++= s"HP:$hp, "
      if (mp != 0) sb ++= s"MP:$mp, "
      if (atk != 0) sb ++= s"Atk:$atk, "
      if (defence != 0) sb ++= s"Def:$defence, "
      if (critRate != 0) sb ++= s"CritR:$critRate, "
      if (critDamage != 0) sb ++= s"CritD:$critDamage, "
      if (dodge != 0) sb ++= s"Dodge:$dodge, "
      if (hit != 0) sb ++= s"Hit:$hit, "
      if (counterDamage != 0) sb ++= s"CounterD:$counterDamage, "
      if (mastery != 0) sb ++= s"Mastery:$mastery, "
      if (counterRate != 0) sb ++= s"CounterR:$counterRate, "
      val str = sb.toString()
      if (str.isEmpty) str else str.dropRight(2)
    }

  }

  // Sum of Equipment potentials Rate xxxx　酷い名前だ
  // ダメージ%は比較には関係なくなるので、除外
  val SEPRCR = 56.7 / 100.0
  val SEPRCD = Stat(critDamage = 68)
  val SEPRAtk = 40 / 100.0

  val SEPRDef = 66.7 / 100.0
  val SEPRHp = 32 / 100.0
  val SEPDef = Stat(defence = 1000)

  val SEPAtkByHP = 10 / 100.0
  val SEPRCRByDef = 12 / 100.0
  // ★2ではクリ率系を優先し、防御%クリ率を優先してみた
  val SEPAtkByMP = (375 - 50) / 100.0


  case class Hero(name: String, stat: Stat, coop: Char, move: Int)(val equipment: List[SetEq], val potentials: List[Stat]) {

    def sumStat: Stat = {
      assert(!equipment.groupBy(_.rare).values.exists(_.size > 1)) // 同じレア度のセット装備を複数セットしてないか一応チェック
      equipment.foldLeft(stat)(_ + _.stat)
    }

    def sumStatWithPote: Stat = {
      potentials.foldLeft(sumStat)(_ + _)
    }

    def sumStatWithEP: Stat = {
      val flatSum = sumStatWithPote + SEPDef
      val addRateSum = flatSum + Stat(critRate = (flatSum.critRate * SEPRCR).toInt) + SEPRCD + Stat(atk = (flatSum.atk * SEPRAtk).toInt) +
        Stat(defence = (flatSum.defence * SEPRDef).toInt) + Stat(hp = (flatSum.hp * SEPRHp).toInt)
      val perfectSum = addRateSum + Stat(defence = (flatSum.defence * SEPRCRByDef).toInt) +
        Stat(atk = (addRateSum.hp * SEPAtkByHP).toInt) +
        Stat(atk = (addRateSum.mp * SEPAtkByMP).toInt)
      perfectSum
    }

    override def toString: String = {
      val sum = sumStatWithEP
      s"$name\n" +
        s"%-10s: %-20s %-14s: ${sum.mp}(+${sum.mp - stat.mp})\n".format("HP", s"${sum.hp}(+${sum.hp - stat.hp})", "MP") +
        s"%-10s: %-20s %-14s: ${sum.defence}/${Hero.defRate(sum.defence)}%%(+${sum.defence - stat.defence})\n".format("Attack", s"${sum.atk}(+${sum.atk - stat.atk})", "Defence") +
        s"%-10s: %-20s %-14s: ${sum.critDamage}%%(+${sum.critDamage - stat.critDamage})\n".format("Crit%", s"${sum.critRate}/${sum.critRate / 100.0}%(+${sum.critRate - stat.critRate})", "CritDamage") +
        s"%-10s: %-20s %-14s: ${sum.counterDamage}/${sum.counterDamage / 100.0}%%(+${sum.counterDamage - stat.counterDamage})\n".format("Counter%", s"${sum.counterRate}/${sum.counterRate / 100.0}(+${sum.counterRate - stat.counterRate})", "CounterDamage") +
        s"%-10s: %-20s %-14s: ${sum.dodge}/?%%(+${sum.dodge - stat.dodge})\n".format("Hit", s"${sum.hit}/${sum.hit / 100.0}%(+${sum.hit - stat.hit})", "Dodge") +
        s"熟練度: ${sum.mastery}(+${sum.mastery - stat.mastery})\t\t移動: $move\t協力: $coop\n" +
        s"${equipment.mkString("　")}\n" +
        s"潜在: ${potentials.mkString("　")}"
    }

  }

  object Hero {
    def defRate(de: Long): Double = {
      def defPow(n: Int): BigDecimal = {
        var ret = BigDecimal(1)
        for (i <- 1 to n) ret *= de
        ret
      }
      val rate = BigDecimal(-0.000000000000000000000005728599) * defPow(6) + BigDecimal(0.000000000000000000392500532243) * defPow(5) - BigDecimal(0.000000000000011303889936997300) *
        defPow(4) + BigDecimal(0.000000000182276425920733000000) * defPow(3) - BigDecimal(0.000001895082217542840000000000) *
        defPow(2) + BigDecimal(0.014494988577674600000000000000) * defPow(1) + BigDecimal(0.454257765543785000000000000000)
      rate.setScale(3, scala.math.BigDecimal.RoundingMode.CEILING).toDouble
    }

    def atkExpectedValue(stat: Stat): Double = {
      val cr = (stat.critRate / 100.0 min 100.0) / 100.0
      val maxAtk = stat.atk * (cr * (stat.critDamage / 100.0 - 1.0) + 1)
      if (stat.mastery >= 10000) maxAtk
      else (maxAtk + maxAtk * (stat.mastery / 10000.0)) / 2
    }

    def durability(stat: Stat): Double = {
      val baseAtk = 10000
      stat.hp / (baseAtk * (100 - defRate(stat.defence)) / 100)
    }

  }

  val krut = Hero(name = "クルート", coop = 'A', move = 4,
    stat = Stat(
      hp = 28517,
      mp = 189,
      atk = 11731,
      defence = 3275,
      critRate = 626,
      critDamage = 130,
      dodge = 644,
      hit = 506,
      counterDamage = 6250,
      mastery = 7600,
      counterRate = 1050)
  ) _

  case class SetEq(name: String, rare: Int, stat: Stat) {
    override def toString: String = s"★$rare$name"
  }

  // Bセット装備の用意
  val setNames = List(/*"清明",*/ "強靭" /*,"明瞭"*/ , "エネルギッシュ", "シャープ" /*,"スピーディ"*/ , "丈夫" /*,"逆転","復讐"*/)
  val rareKind = 5
  // weapon
  val sword = List(Stat(atk = 317, hit = 320), Stat(atk = 752, hit = 399), Stat(atk = 1304, hit = 484), Stat(atk = 2024, hit = 575), Stat(atk = 2966, hit = 672))
  val arrow = List(Stat(atk = 292, critRate = 320), Stat(atk = 691, critRate = 399), Stat(atk = 1200, critRate = 484), Stat(atk = 1862, critRate = 575), Stat(atk = 2756, critRate = 672))
  val mace = List(Stat(atk = 355, counterDamage = 1120), Stat(atk = 842, counterDamage = 1368), Stat(atk = 1460, counterDamage = 1628), Stat(atk = 2266, counterDamage = 1900), Stat(atk = 3356, counterDamage = 2184))
  // armor
  val armor = List(Stat(hp = 1210, defence = 320), Stat(hp = 2929, defence = 399), Stat(hp = 5106, defence = 484), Stat(hp = 7930, defence = 575), Stat(hp = 11740, defence = 672))
  val shoes = List(Stat(hp = 1113, dodge = 320), Stat(hp = 2695, dodge = 399), Stat(hp = 4697, dodge = 484), Stat(hp = 7296, dodge = 575), Stat(hp = 10801, dodge = 672))
  val shield = List(Stat(hp = 1355, counterRate = 160), Stat(hp = 3280, counterRate = 228), Stat(hp = 5718, counterRate = 308), Stat(hp = 8882, counterRate = 400), Stat(hp = 13149, counterRate = 504))
  // accessory
  val necklace = List(Stat(atk = 159, mastery = 1184), Stat(atk = 376, mastery = 1406), Stat(atk = 652, mastery = 1628), Stat(atk = 1012, mastery = 1850), Stat(atk = 1498, mastery = 2072))
  val ring = List(Stat(critRate = 272, hit = 272), Stat(critRate = 339, hit = 339), Stat(critRate = 411, hit = 411), Stat(critRate = 489, hit = 489), Stat(critRate = 571, hit = 571))
  val broach = List(Stat(hp = 605, defence = 224), Stat(hp = 1465, defence = 279), Stat(hp = 2553, defence = 339), Stat(hp = 3965, defence = 403), Stat(hp = 5870, defence = 470))
  // set effect
  val seimei = List(Stat(mp = 15), Stat(mp = 20), Stat(mp = 25), Stat(mp = 30), Stat(mp = 35))
  val kyojin = List(Stat(atk = 190), Stat(atk = 451), Stat(atk = 782), Stat(atk = 1214), Stat(atk = 1798))
  val meiryo = List(Stat(hit = 400), Stat(hit = 500), Stat(hit = 600), Stat(hit = 700), Stat(hit = 800))
  val energetic = List(Stat(hp = 726), Stat(hp = 1757), Stat(hp = 3063), Stat(hp = 4758), Stat(hp = 7044))
  val sharp = List(Stat(critRate = 400), Stat(critRate = 500), Stat(critRate = 600), Stat(critRate = 700), Stat(critRate = 800))
  val speedy = List(Stat(dodge = 400), Stat(dodge = 500), Stat(dodge = 600), Stat(dodge = 700), Stat(dodge = 800))
  val jobu = List(Stat(defence = 600), Stat(defence = 700), Stat(defence = 800), Stat(defence = 900), Stat(defence = 1000))
  val gyakuten = List(Stat(counterRate = 200), Stat(counterRate = 250), Stat(counterRate = 300), Stat(counterRate = 350), Stat(counterRate = 400))
  val hukusyu = List(Stat(counterDamage = 1400), Stat(counterDamage = 1450), Stat(counterDamage = 1500), Stat(counterDamage = 1550), Stat(counterDamage = 1600))

  // 潜在能力
  val hpPote = List(Stat(hp = 800), Stat(hp = 740), Stat(hp = 660), Stat(hp = 560), Stat(hp = 420))
  val mpPote = List(Stat(mp = 30), Stat(mp = 28), Stat(mp = 26), Stat(mp = 24), Stat(mp = 21))
  val atkPote = List(Stat(atk = 250), Stat(atk = 235), Stat(atk = 215), Stat(atk = 190), Stat(atk = 155))
  val defPote = List(Stat(defence = 400), Stat(defence = 380), Stat(defence = 360), Stat(defence = 340), Stat(defence = 310))
  val critRPote = List(Stat(critRate = 400), Stat(critRate = 380), Stat(critRate = 360), Stat(critRate = 340), Stat(critRate = 310))
  val dodgePote = List(Stat(dodge = 400), Stat(dodge = 380), Stat(dodge = 360), Stat(dodge = 340), Stat(dodge = 310))
  val hitPote = List(Stat(hit = 400), Stat(hit = 380), Stat(hit = 360), Stat(hit = 340), Stat(hit = 310))
  val counterDPote = List(Stat(counterDamage = 1600), Stat(counterDamage = 1400), Stat(counterDamage = 1200), Stat(counterDamage = 1000), Stat(counterDamage = 700))
  val masteryPote = List(Stat(mastery = 400), Stat(mastery = 380), Stat(mastery = 360), Stat(mastery = 340), Stat(mastery = 310))
  val counterRPote = List(Stat(counterRate = 400), Stat(counterRate = 380), Stat(counterRate = 360), Stat(counterRate = 340), Stat(counterRate = 310))
  //  val damagePote =	List(Stat(damage=200)	,Stat(damage=180)	,Stat(damage=160)	,Stat(damage=140),	Stat(damage=110))
  //  val iDamagePote =	List(Stat(iDamage=200)	,Stat(iDamage=180),	Stat(iDamage=160)	,Stat(iDamage=140)	,Stat(iDamage=110))
  val potentials = List(hpPote, mpPote, atkPote, defPote, critRPote /*, dodgePote, hitPote, counterDPote, masteryPote, counterRPote*/)
  val potentialCombination = potentials.flatMap(p => List(p.head, p.head, p.head, p.head, p.head)).combinations(5).toList

  val setEqs: List[SetEq] = for {
    name <- setNames
    rare <- 0 until rareKind
  } yield {
    name match {
      case "清明"      => SetEq(name, rare + 1, sword(rare) + armor(rare) + necklace(rare) + seimei(rare))
      case "強靭"      => SetEq(name, rare + 1, sword(rare) + shoes(rare) + necklace(rare) + kyojin(rare))
      case "明瞭"      => SetEq(name, rare + 1, sword(rare) + armor(rare) + ring(rare) + meiryo(rare))
      case "エネルギッシュ" => SetEq(name, rare + 1, arrow(rare) + armor(rare) + broach(rare) + energetic(rare))
      case "シャープ"    => SetEq(name, rare + 1, arrow(rare) + shoes(rare) + ring(rare) + sharp(rare))
      case "スピーディ"   => SetEq(name, rare + 1, arrow(rare) + shoes(rare) + broach(rare) + speedy(rare))
      case "丈夫"      => SetEq(name, rare + 1, mace(rare) + shield(rare) + broach(rare) + jobu(rare))
      case "逆転"      => SetEq(name, rare + 1, mace(rare) + shield(rare) + necklace(rare) + gyakuten(rare))
      case "復讐"      => SetEq(name, rare + 1, mace(rare) + shield(rare) + ring(rare) + hukusyu(rare))
    }
  }
  val eachRareEqs = setEqs.groupBy(_.rare)

  // 数に余裕があるので総当り
  val kruts = for {
    rare1 <- eachRareEqs(1)
    rare2 <- eachRareEqs(2)
    rare3 <- eachRareEqs(3)
    rare4 <- eachRareEqs(4)
    rare5 <- eachRareEqs(5)
    potential <- potentialCombination
  } yield {
    krut(List(rare1, rare2, rare3, rare4, rare5), potential)
  }

  //  val top10AtkEVKruts = kruts map (k => (k, Hero.atkExpectedValue(k.sumStat))) sortBy (ktp => -ktp._2) take 10
  //  println(top10AtkEVKruts.mkString("\n\n"))

  //  val top10toughKruts = kruts map (k => (k, Hero.durability(k.sumStatWithEP))) sortBy (ktp => -ktp._2) take 10
  //  println(top10toughKruts.mkString("\n\n"))

  val top10BalanceKruts = kruts map { k =>
    val sswep = k.sumStatWithEP
    val dura = Hero.durability(sswep)
    val atkev = Hero.atkExpectedValue(sswep)
    (k, dura, atkev, (dura * 1771 + atkev) / 2)
  } sortBy (ktp => -ktp._4) take 10
  println(top10BalanceKruts.mkString("\n\n"))

}
