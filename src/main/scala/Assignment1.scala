
object Assignment1 {
  val ENGLISH_LETTER_FREQ = 0.065
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

  val SUM_OF_ENG_LETTER_FREQ = ENGLISH_LETTER_FREQS.map { case (x, y) => y * y }.sum

  def main(args: Array[String]) {

    println(SUM_OF_ENG_LETTER_FREQ)

    val cipherHex = "F96DE8C227A259C87EE1DA2AED57C93FE5DA36ED4EC87EF2C63AAE5B9A7EFFD673BE4ACF7BE8923CAB1ECE7AF2DA3DA44FCF7AE29235A24C963FF0DF3CA3599A70E5DA36BF1ECE77F8DC34BE129A6CF4D126BF5B9A7CFEDF3EB850D37CF0C63AA2509A76FF9227A55B9A6FE3D720A850D97AB1DD35ED5FCE6BF0D138A84CC931B1F121B44ECE70F6C032BD56C33FF9D320ED5CDF7AFF9226BE5BDE3FF7DD21ED56CF71F5C036A94D963FF8D473A351CE3FE5DA3CB84DDB71F5C17FED51DC3FE8D732BF4D963FF3C727ED4AC87EF5DB27A451D47EFD9230BF47CA6BFEC12ABE4ADF72E29224A84CDF3FF5D720A459D47AF59232A35A9A7AE7D33FB85FCE7AF5923AA31EDB3FF7D33ABF52C33FF0D673A551D93FFCD33DA35BC831B1F43CBF1EDF67F0DF23A15B963FE5DA36ED68D378F4DC36BF5B9A7AFFD121B44ECE76FEDC73BE5DD27AFCD773BA5FC93FE5DA3CB859D26BB1C63CED5CDF3FE2D730B84CDF3FF7DD21ED5ADF7CF0D636BE1EDB79E5D721ED57CE3FE6D320ED57D469F4DC27A85A963FF3C727ED49DF3FFFDD24ED55D470E69E73AC50DE3FE5DA3ABE1EDF67F4C030A44DDF3FF5D73EA250C96BE3D327A84D963FE5DA32B91ED36BB1D132A31ED87AB1D021A255DF71B1C436BF479A7AF0C13AA14794"
    val cipherAscii = cipherHex.grouped(2).toList.map(Integer.parseInt(_, 16))

    val frequenciesSquared = for (keyLength <- 1 to 13) yield computeFrequenciesSquared(keyLength, cipherAscii)
    val keyLength = frequenciesSquared.maxBy { case (x, y) => y }._1

    println(s"keyLength:$keyLength")
    val key = (1 to keyLength).map(keyIndex => maxSumOfFreqs(keyLength, keyIndex, cipherAscii)).toList

    println(s"key:$key")

    val result = decrypt(key, cipherAscii).toList
    println(result.map { _.toChar } mkString)

  }

  def decrypt(key: List[Int], cipherAscii: List[Int]) = {
    cipherAscii.grouped(key.size).map { partialCipher =>
      {
        val zipped = partialCipher.zip(key)
        zipped.map { case (x, y) => x ^ y }
      }
    }.flatten
  }

  def maxSumOfFreqs(keyLength: Int, keyIndex: Int, cipherAscii: List[Int]) = {
    val stream = IthCipherStream(keyLength, keyIndex, cipherAscii)
    val plainTexts = (0 to 255).map { x => (x, xor(x, stream)) }
    val filteredPlainTexts = plainTexts.filter { case (x, text) => checkBetween32And127(text) }

    val sumOfFreqs = filteredPlainTexts.map {
      case (x, text) => {
        val freqs = frequencies(text.filter(isLowerCase))
        //        val summationOfFreq = freqs.map { case (c, q) => q * ENGLISH_LETTER_FREQS(c) }.sum
        val summationOfFreq = freqs.map {
          case (c, q) => {
            val delta = q-ENGLISH_LETTER_FREQS(c)
            delta*delta
          }
        }.sum
        (x, math.sqrt(summationOfFreq))
      }
    }

    val result = sumOfFreqs.filterNot{case (x, y) => y==0.0 }.minBy { case (x, y) => y }._1
    println(s"keyIndex:$keyIndex, result:$result \n")
    result
  }
  def isLowerCase(x: Int) = 97 <= x && x <= 122
  def checkBetween32And127(stream: List[Int]) = stream.forall { x => 32 <= x && x < 127 }

  def xor(b: Int, stream: List[Int]) = {
    stream.map { x => x ^ b }
  }

  def computeFrequenciesSquared(keyLength: Int, cipherAscii: List[Int]) = {
    val samples = cipherAscii.grouped(keyLength).map(_.head).toList
    val freq = frequencies(samples)
    (keyLength, freq.map { case (x, y) => y * y }.toList.sum)
  }

  def frequencies(stream: List[Int]) = {
    val streamSize = stream.size.toDouble
    val grouped = stream.groupBy(x => x)
    grouped.map { case (x, ys) => (x, ys.size / streamSize) }
  }

  def IthCipherStream(keyLength: Int, i: Int, cipherAscii: List[Int]) = {
    cipherAscii.drop(i - 1).grouped(keyLength).map(xs => xs.head).toList
  }
}