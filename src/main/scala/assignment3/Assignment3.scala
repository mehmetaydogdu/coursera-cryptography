package assignment3

/**
 * @author Mehmet Aydogdu
 */
object Assignment3 {
  val blockLength = 16
  val challengeCipherText = "9F0B13944841A832B2421B9EAF6D9836813EC9D944A5C8347A7CA69AA34D8DC0DF70E343C4000A2AE35874CE75E64C31"
  val oracle = new Oracle

  def main(args: Array[String]) {
    oracle.connect()

    val cipherAscii = challengeCipherText.grouped(2).toList.map { x => Integer.parseInt(x, 16) }
    val (iv, tail) = cipherAscii.splitAt(16)
    val tailGrouped = tail.grouped(blockLength).toList
    val middle = tailGrouped.drop(2).flatten
    val end = tailGrouped.takeRight(2)
    val c1 = end(0)
    val c2 = end(1)

    println(s"iv:$iv")
    println(s"middle:$middle")
    println(s"c1:$c1")
    println(s"c2:$c2")
    println(s"cipherAscii:$cipherAscii")

    //Iterate over c1 and change its bytes starting from left-most till oracle fails
    val c1Stream = (0 to c1.size).toStream.map { i =>
      val (start, x :: end) = c1.splitAt(i)
      val xPrime = x ^ 1
      val c1Prime = start.map(x => xPrime) ::: (xPrime :: end)
      val manipulatedIv = iv ::: middle ::: c1Prime ::: c2
      (i, manipulatedIv)
    }
    val notValidCipher = c1Stream.find { case (i, cipher) => !isValid(cipher) }.get
    println(notValidCipher)

    val padding = blockLength - notValidCipher._1
    println(s"padding is:$padding")
//
//    val c2Encoded = findNextByte(padding, iv, c1, c2)
//    val result1 = c2Encoded.map(_.toChar)
//    println(s"c2Result: $result1")
    
    val c1Encoded = findNextByte(0, Nil, iv, c1)
    println("c1Encoded: "+c1Encoded.map(_.toChar))
//    val result = (c1Encoded ::: c2Encoded).map(_.toChar)
//    println(s"result:${result.mkString("")}")
    oracle.disconnect()

  }

  def findNextByte(padding: Int, head: List[Int], iv: List[Int], c: List[Int]) = {
    def inner(acc: List[Int], padding: Int, head: List[Int], iv: List[Int], c: List[Int]):List[Int] = {
      if (padding >= blockLength) {
        acc
      } else {
        println(s"iv:$iv")
        println(s"c:$c")
        println(s"padding:$padding")
        val newPadding = padding + 1
        val targetIndex = blockLength - padding
        val tail1 = iv.drop(targetIndex)
        val tail = iv.drop(targetIndex).map(x => x ^ padding ^ newPadding)
        val start = iv.take(targetIndex - 1)
        val (validIVElem, ivPrime) = (0 to 255).toStream.map(x => (x, start ::: (x :: tail))).find {
          case (x, ivPrime) =>
            isValid(head ::: ivPrime ::: c )
        }.get
        val encoded = validIVElem ^ newPadding ^ iv(targetIndex - 1)
        println(s"ENCODED:$encoded, ${encoded.toChar}")
        inner(encoded::acc, padding+1, head, ivPrime, c)
      }
    }
    inner(Nil, padding, head, iv, c)

  }

  def isValid(cipher: List[Int]) = {

    val rc = oracle.send(cipher.toArray, cipher.size/blockLength)
    println(s"Oracle : $rc, ${cipher.grouped(blockLength).toList mkString " "}")
    rc == 1
  }
}

import java.net.Socket
import java.io.OutputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.IOException
import java.util.Arrays
import scala.collection.JavaConversions._
import scala.collection.JavaConversions

/**
 * @author Mehmet Aydogdu
 *
 */
class Oracle {
  //  val hostname = "52.7.91.172"
  val hostname = "52.25.162.51"

  val port = 80

  var socket: Socket = _
  var out: OutputStream = _
  var in: BufferedReader = _

  def connect(): Int = {

    var result = 0
    try {
      socket = new Socket(hostname, port)
      out = socket.getOutputStream()
      in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
      //      System.out.println("Connected to server successfully.")
    } catch {
      case e: IOException => {
        e.printStackTrace()
        result = -1
      }
    }
    result
  }

  def disconnect(): Int = {
    var result = 0
    try {
      socket.close()
      //      System.out.println("Connection closed successfully.")
    } catch {
      case e: IOException => {
        e.printStackTrace()
        result = -1
      }
    }
    result
  }

  // Packet Structure: < num_blocks(1) || ciphertext(16*num_blocks) || null-terminator(1) >
  def send(ctext: Array[Int], num_blocks: Int): Int = {
    val message = Array[Byte](num_blocks.toByte) ++ ctext.map { _.toByte } ++ Array[Byte](0.toByte)
    val recvbit = Array[Char](' ', ' ')
    try {
      out.write(message);
      out.flush();
      in.read(recvbit, 0, 2);
      return Integer.valueOf(new String(recvbit).replaceAll("\0", ""));
    } catch {
      case e: IOException => e.printStackTrace()
    }
    return -1;
  }

}