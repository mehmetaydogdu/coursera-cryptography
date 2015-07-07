

/**
 * @author Mehmet Aydogdu
 */
object Assignment2 {

  val ENGLISH_LETTER_FREQS = Map(
    'a' -> 0.08167,
    'b' -> 0.01492,
    'c' -> 0.02782,
    'd' -> 0.04253,
    'e' -> 0.12702,
    'f' -> 0.02228,
    'g' -> 0.02015,
    'h' -> 0.06094,
    'i' -> 0.06966,
    'j' -> 0.00153,
    'k' -> 0.00772,
    'l' -> 0.04025,
    'm' -> 0.02406,
    'n' -> 0.06749,
    'o' -> 0.07507,
    'p' -> 0.01929,
    'q' -> 0.00095,
    'r' -> 0.05987,
    's' -> 0.06327,
    't' -> 0.09056,
    'u' -> 0.02758,
    'v' -> 0.00978,
    'w' -> 0.02361,
    'x' -> 0.00150,
    'y' -> 0.01974,
    'z' -> 0.00074).map { case (x, y) => (x.toInt, y) }

  val spaceAsciiValue = 32
  val cipherHexs = List(
    "BB3A65F6F0034FA957F6A767699CE7FABA855AFB4F2B520AEAD612944A801E",
    "BA7F24F2A35357A05CB8A16762C5A6AAAC924AE6447F0608A3D11388569A1E",
    "A67261BBB30651BA5CF6BA297ED0E7B4E9894AA95E300247F0C0028F409A1E",
    "A57261F5F0004BA74CF4AA2979D9A6B7AC854DA95E305203EC8515954C9D0F",
    "BB3A70F3B91D48E84DF0AB702ECFEEB5BC8C5DA94C301E0BECD241954C831E",
    "A6726DE8F01A50E849EDBC6C7C9CF2B2A88E19FD423E0647ECCB04DD4C9D1E",
    "BC7570BBBF1D46E85AF9AA6C7A9CEFA9E9825CFD5E3A0047F7CD009305A71E")

  val cipherAscii = cipherHexs.map(_.grouped(2).toList.map(Integer.parseInt(_, 16)))

  def main(args: Array[String]) {
    val transposed = cipherAscii.transpose
    val candidateKeys = solve(transposed).map { keys =>
      keys match {
        case Nil => Nil //(0 to 255).toList
        case _ => keys.distinct
      }
    }
    val filteredCandidateKeys = candidateKeys.zip(transposed) map {
      case (keys, ciphers) =>
        val filteredKeys = keys.filter(key => ciphers.forall { c => isValidKey(key, c) })
        val groupByProbability = filteredKeys.map { key =>
          val text = ciphers.map { c => c ^ key }
          val freqs = frequencies(text.filter(isLowerCase))
          val summationOfFreq = freqs.map {
            case (c, q) => {
              val delta = q - ENGLISH_LETTER_FREQS(c)
              delta * delta
            }
          }.sum

          (key, math.sqrt(summationOfFreq))
        }
        if (filteredKeys.isEmpty) None
        else {
          val bestKey = groupByProbability.minBy { case (x, y) => y }._1
          Some(bestKey)
        }

    }

    println(s"key:$candidateKeys")
    println(s"key:$filteredCandidateKeys")

    val resultInt = cipherAscii.map(text => {
      println(text.zip(filteredCandidateKeys))
      text.zip(filteredCandidateKeys).map {
        case (c, key) => {
          key match {
            case None => 63
            case Some(k) => c ^ k
          }
        }
      }
    })

    val resultChar = resultInt.map { text => text.map(_.toChar) mkString } mkString "\n"
    println(s"result:\n$resultChar")

  }

  def isLowerCase(x: Int) = 97 <= x && x <= 122

  def frequencies(stream: List[Int]) = {
    val streamSize = stream.size.toDouble
    val grouped = stream.groupBy(x => x)
    grouped.map { case (x, ys) => (x, ys.size / streamSize) }
  }

  def solve(cipherTransposed: List[List[Int]]) = {

    cipherTransposed.map { cipher =>
      val subsets3 = cipher.combinations(3).toList
      val matchedSubsets3 = subsets3.filter { y =>

        val subsets2 = subsets2of3(y).toList
        hasRule1(subsets2)
      }

      findCandidateKeys(Nil, matchedSubsets3, cipher)
    }
  }

  def findCandidateKeys(acc: List[Int], subsets: List[List[Int]], cipher: List[Int]): List[Int] = {
    subsets match {
      case Nil => acc
      case x :: xs => {
        val key = x match {
          case set3 => {
            val m1 = set3(0)
            val m2 = set3(1)
            val m3 = set3(2)
            val key = {
              if ((m1 ^ m2) == 0) m3 ^ spaceAsciiValue
              else if ((m1 ^ m3) == 0) m2 ^ spaceAsciiValue
              else m1 ^ spaceAsciiValue
            }
            println(s"m1:$m1, m2:$m2, m3:$m3, key:$key")
            println(s"m1m2:${toBinaryString(m1 ^ m2)}")
            println(s"m1m3:${toBinaryString(m1 ^ m3)}")
            println(s"m2m3:${toBinaryString(m2 ^ m3)}")
            key
          }
        }
        findCandidateKeys(key :: acc, xs, cipher)
      }
    }
  }

  def isValidKey(key: Int, c: Int) = {
    val candidate = c ^ key
    32 <= candidate && candidate <= 122 &&
      !(48 <= candidate && candidate <= 57) && //Not number
      candidate != 35 && candidate != 36 && candidate != 37 && candidate != 38 && candidate != 40 && candidate != 41 &&
      candidate != 94 && candidate != 60 && candidate != 61 && candidate != 62 && candidate != 94 && candidate != 95 && candidate != 96 //&& candidate!=42 && candidate!=92 && candidate!=91 && candidate!=94
  }
  def subsets2of3(y: List[Int]) = List(List(y(0), y(1)), List(y(0), y(2)), List(y(1), y(2))).toIterator

  def hasRule1(subsets2: List[List[Int]]) = {
    val x = subsets2.filter(has2Letters)
    val y = subsets2.filter(has1Letter1Space)
    x.size == 1 && y.size == 2
  }

  def has2Letters(subset: List[Int]) = {
    subset match {
      case Nil => false
      case x :: Nil => false
      case x :: y :: xs => toBinaryString(x ^ y).startsWith("00")
    }
  }
  def has1Letter1Space(subset: List[Int]) = {
    subset match {
      case Nil => false
      case x :: Nil => false
      case x :: y :: xs => toBinaryString(x ^ y).startsWith("01")
    }
  }

  def toBinaryString(i: Int) = {
    String.format("%8s", i.toBinaryString).replace(" ", "0")
  }
}