import scala.annotation.tailrec
import scala.collection.immutable.{NumericRange, Seq}
import scala.collection.mutable.ArrayBuffer

object Day5Take2 {
  def main(args: Array[String]): Unit = {
    val timer = System.currentTimeMillis

    val testInput = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
    val realInput = "seeds: 3037945983 743948277 2623786093 391282324 195281306 62641412 769611781 377903357 2392990228 144218002 1179463071 45174621 2129467491 226193957 1994898626 92402726 1555863421 340215202 426882817 207194644\n\nseed-to-soil map:\n3078006360 2182201339 30483272\n803630304 624445326 165226844\n2393736333 2745251526 281120946\n717936870 789672170 85693434\n598717319 410599330 27984688\n3999095007 2024628810 157572529\n3605588191 3026372472 22322803\n3555659576 2678166775 3396919\n968857148 438584018 1780307\n3216227818 2212684611 87459567\n2302084376 4122083708 91651957\n970637455 0 188112122\n507182228 299146916 40412346\n1372302034 1689624457 202945009\n1370123632 191483770 2178402\n324787204 193662172 105484744\n3116425470 2671328191 6838584\n626702007 875365604 82756204\n1575247043 978774853 317322423\n3134996187 4213735665 81231631\n2024628810 2681563694 63687832\n714565222 188112122 3371648\n547594574 1620884480 51122745\n3529388087 3374604163 26271489\n709458211 973428243 5107011\n2713008276 3985570976 98361735\n2088316642 3048695275 213767734\n3627910994 2300144178 371184013\n2674857279 4083932711 38150997\n1229789645 958121808 15306435\n4156667536 3328662676 45941487\n0 1296097276 324787204\n3108489632 3320726838 7935838\n4202609023 3667512001 92358273\n1352266801 978535254 239599\n1352506400 1672007225 17617232\n1245096080 440364325 107170721\n2811370011 3400875652 266636349\n430271948 547535046 76910280\n1158749577 339559262 71040068\n3559056495 3262463009 46531696\n3123264054 3308994705 11732133\n3303687385 3759870274 225700702\n\nsoil-to-fertilizer map:\n2937874770 2957653952 339980892\n1886469734 2145122669 192293654\n3277855662 822424488 19779182\n2622882196 2393077006 314992574\n3449876679 3769116301 525850995\n583550735 842203670 1302918999\n2145755543 345297835 477126653\n2078763388 2890661797 66992155\n2650514 2708069580 182592217\n0 2337416323 2650514\n530540566 2340066837 53010169\n185242731 0 345297835\n3975727674 3449876679 319239622\n\nfertilizer-to-water map:\n861477134 5168332 68211907\n136969509 2229711837 29094441\n2823248929 1150509810 118368045\n3678888284 3073610919 53498438\n3948051821 3682691325 96234592\n1302827191 2387840795 504257794\n1198743248 1926818347 104083943\n1807084985 1104177008 46332802\n2143096098 619653304 259805223\n2063436946 2385211148 2629647\n2066066593 445026117 35759449\n358008423 537865723 81787581\n621204445 0 5168332\n2724438904 1861632296 65186051\n1853417787 2258806278 126404870\n3933311080 4141091197 14740741\n851739278 2892098589 9737856\n4044286413 3029323079 44287840\n1979822657 1778018007 83614289\n2101826042 2084781230 3070511\n4088574253 4268409625 26557671\n929689041 111346117 211974050\n3566310597 4155831938 112577687\n439796004 2030902290 53878940\n166063950 1490707297 191944473\n8760514 888219041 128208995\n3794695843 3778925917 57203243\n3029323079 3127109357 409045756\n2792635116 77722143 30613813\n3438368835 4013149435 127941762\n3732386722 3620382204 62309121\n2402901321 1682651770 95366237\n0 879458527 8760514\n493674944 2901836445 39780529\n3851899086 3536155113 81411994\n2498267558 1268877855 221829442\n4117947021 3836129160 177020275\n2789624955 108335956 3010161\n1141663091 480785566 57080157\n2104896553 406826572 38199545\n533455473 1016428036 87748972\n626372777 2087851741 141860096\n2720097000 73380239 4341904\n4115131924 3617567107 2815097\n768232873 323320167 83506405\n\nwater-to-light map:\n3846882465 367033980 98093832\n1878565977 3292746518 62917983\n4255729420 661438934 39237876\n469590509 2191298319 301681796\n381948234 1999013894 87642275\n3688496086 199351627 156562666\n1300818753 2086656169 104642150\n806539912 2798447654 224466318\n1265336919 355914293 11119687\n1405460903 1914042148 28882526\n2577391070 1942924674 56089220\n3680239306 4136990116 8256780\n1941483960 700676810 607954854\n3845058752 3022913972 1823713\n4239658038 1308631664 16071382\n2566162195 4254580741 11228875\n1671792383 3831845903 10462472\n3944976297 3842308375 294681741\n3290662499 3160062910 132683608\n2549438814 1324703046 16723381\n3423346107 1341426427 27108304\n1031006230 3355664501 234330689\n1276456606 4145246896 24362147\n3450454411 54538430 144813197\n1682254855 465127812 196311122\n54538430 1403802338 272790856\n2633480290 2492980115 305467539\n3595267608 4169609043 84971698\n3242064105 3644614138 48598394\n3077581200 4265809616 29157680\n771272305 1368534731 35267607\n1434343429 1676593194 237448954\n327329286 3589995190 54618948\n3106738880 3024737685 135325225\n2938947829 3693212532 138633371\n\nlight-to-temperature map:\n2777813298 2971073270 586210802\n1687968665 0 334152507\n4159107034 3882460035 135860262\n0 2095520416 192800212\n3640671099 3557284072 3145370\n2455782705 3560429442 322030593\n2022121172 1272848785 266199456\n773517036 914869331 357979454\n1131496490 1539048241 556472175\n3364024100 4018320297 60669366\n3643816469 2455782705 515290565\n192800212 334152507 580716824\n3424693466 4078989663 215977633\n\ntemperature-to-humidity map:\n4072523312 605654847 17750681\n1174610018 540191835 65463012\n2038455907 3792024734 100202248\n2539396783 866566556 128459181\n96342672 2296045868 14715058\n3827330744 1522255720 106701221\n3816190028 4081148893 11140716\n1706101724 3892226982 188921911\n3780839952 623405528 35350076\n765616949 1813669629 408993069\n4225769488 3778728770 13295964\n2752105545 1645897858 167771771\n2138658155 1121517092 400738628\n4239065452 4155853973 55901844\n3934031965 96342672 35726394\n3005272654 658755604 22724553\n3989311833 4211755817 83211479\n280430452 3186777866 320785315\n111057730 2310760926 65268270\n176326000 3659551759 104104452\n1895023635 1628956941 16940917\n4093334384 3507563181 132435104\n3027997207 132069066 179421352\n1477400307 311490418 228701417\n2934949875 2225723089 70322779\n601215767 681480157 100836818\n2919877316 3763656211 15072559\n3969758359 3639998285 19553474\n3207418559 2613356473 573421393\n4090273993 2222662698 3060391\n1911964552 995025737 126491355\n2667855964 782316975 84249581\n1240073030 2376029196 237327277\n702052585 4092289609 63564364\n\nhumidity-to-location map:\n2848734682 2982177676 22285660\n3380476660 3717224958 24199873\n3201930685 734568132 100088122\n764851360 4087339561 71173655\n188169313 2953711255 28466421\n3189375901 2832231336 12554784\n3369909102 47909639 10567558\n47909639 3741424831 99762378\n2871020342 58477197 7400020\n3042878026 3409715295 146497875\n1196348942 2734551883 97679453\n3418711171 3387790447 21924848\n1587973141 573552831 65833150\n1121006696 889063447 75342246\n1294028395 567796360 5756471\n3302018807 499906065 67890295\n2915035031 2921050411 32660844\n1982422286 3064299481 301982155\n704786709 4084864539 2475022\n299076626 834656254 54407193\n3623423724 2182055199 207702290\n388851709 2881400789 39649622\n147672017 3592293988 40497296\n2947695875 639385981 95182151\n707261731 3556213170 36080818\n2284404441 2389757489 138657919\n353483819 2146687309 35367890\n2519626540 441189704 58319568\n743342549 3366281636 21508811\n2878420362 2844786120 36614669\n216635734 4212526404 82440892\n3440636019 1235194267 182787705\n2577946108 964405693 270788574\n1453221956 3877915244 70435137\n836025015 1861705628 284981681\n1299784866 499509272 396793\n1859942766 3948350381 122479520\n2423062360 3004463336 59836145\n3404676533 4070829901 14034638\n1399208768 4158513216 54013188\n1300181659 342162595 99027109\n1523657093 3652908910 64316048\n3851243640 1417981972 443723656\n3831126014 3632791284 20117626\n1653806291 2528415408 206136475\n428501331 65877217 276285378\n2482898505 3841187209 36728035"

    val input = realInput

    val seedsRegex = """seeds:([\d\s]+)\n""".r
    val regex = """(\n[\d ]+)+""".r

    case class SourceDestinationMap(sourceStart: Long, destinationStart: Long, rangeSize: Long)

    val seeds = seedsRegex.findAllIn(input).matchData.toSeq.head.group(1).trim.split(" ").map(_.toLong)

    val seedsPart2 = ArrayBuffer.empty[NumericRange[Long]]

    def generatePart2Seeds(): Unit = {
      for (i <- seeds.indices by 2) {
        val range = (seeds(i) to seeds(i) + seeds(i + 1) - 1 by 1)
        seedsPart2.addOne(range)
      }
    }

    generatePart2Seeds()

    val mapInputs = regex.findAllIn(input.substring(input.indexOf("seed-to-soil map:"))).matchData.toList.map(_.group(0))

    def parseMap(index: Int): Seq[SourceDestinationMap] = {
      mapInputs(index).trim.split("\n").map { row =>
        val split = row.split(" ").map(_.toLong)
        SourceDestinationMap(split(1), split(0), split(2))
      }
    }

    val maps = (0 to 6).map { index =>
      parseMap(index)
    }

    @tailrec
    def mapSeeds(input: Seq[Long], depth: Int): Seq[Long] = {
      val map = maps(depth)
      val output = input.map { inputItem =>
        val matched = map.find { sdm =>
          inputItem >= sdm.sourceStart && inputItem < sdm.sourceStart + sdm.rangeSize
        }
        if (matched.isEmpty) {
          inputItem
        } else {
          inputItem - matched.head.sourceStart + matched.head.destinationStart
        }
      }
      if (depth == 6) {
        output
      } else {
        mapSeeds(output, depth + 1)
      }
    }

    val seedCandidateRunOutput = seedsPart2.zipWithIndex.map { tuple =>
      val result = mapSeeds(tuple._1, 0)
      (result.min, tuple._2)
    }.minBy(_._1)

    val windowSize = 1
    val seedCandidateIndex = seedCandidateRunOutput._2
    val seed = seedsPart2(seedCandidateIndex)
    val upperBound = seed.end
    var resultToBeat = mapSeeds(List(seed.end), 0).min
    val rangeSize = seed.size
    var slidingUpperBound = upperBound
    var resultUpperBound = slidingUpperBound

    while (slidingUpperBound >= seed.start) {
      val result = mapSeeds(((slidingUpperBound - rangeSize) to (slidingUpperBound) by windowSize).toList, 0).min
      if (result < resultToBeat) {
        resultToBeat = result
        resultUpperBound = slidingUpperBound
        slidingUpperBound -= rangeSize
      } else {
        slidingUpperBound = 0
      }
    }

    val result = mapSeeds(((resultUpperBound - rangeSize - windowSize) to resultUpperBound).toList, 0).min
    val duration = System.currentTimeMillis() - timer

    println(result)
    println(duration)
  }


}