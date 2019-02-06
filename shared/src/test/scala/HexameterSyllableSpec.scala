package edu.holycross.shot.greek
import org.scalatest.FlatSpec
import edu.holycross.shot.greek._



class HexameterSyllableSpec extends FlatSpec {

	"A hexameter syllable" should "consruct" in {
		val syll:LiteraryGreekString = LiteraryGreekString("mh")
		val quant = LongSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		assert (hs.lgs.ascii == "mh")
		assert (hs.quantity == LongSyllableType)
	}

	it should "consruct with a short" in {
		val syll:LiteraryGreekString = LiteraryGreekString("a")
		val quant = ShortSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		assert (hs.lgs.ascii == "a")
		assert (hs.quantity == ShortSyllableType)
	}

	it should "consruct with an anceps" in {
		val syll:LiteraryGreekString = LiteraryGreekString("ke")
		val quant = AncepsSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		assert (hs.lgs.ascii == "ke")
		assert (hs.quantity == AncepsSyllableType)
	}

	it should "pretty print a short syllable" in {
		val syll:LiteraryGreekString = LiteraryGreekString("a")
		val quant = ShortSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		val expected:String = "α\u0306"
		println(hs.toString)
		assert (hs.toString == expected)
	}

	it should "pretty print a long syllable" in {
		val syll:LiteraryGreekString = LiteraryGreekString("mh")
		val quant = LongSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		val expected:String = "μη\u0305"
		println(hs.toString)
		assert (hs.toString == expected)
	}

	it should "pretty print an anceps syllable" in {
		val syll:LiteraryGreekString = LiteraryGreekString("ke")
		val quant = AncepsSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		val expected:String = "κε\u033D"
		println(hs.toString)
		assert (hs.toString == expected)
	}

	it should "pretty print a short" in {
		val syll:LiteraryGreekString = LiteraryGreekString("a")
		val quant = ShortSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		val expected:String = "\u23D1"
		println(hs.toMeter)
		assert (hs.toMeter == expected)
	}

	it should "pretty print a long" in {
		val syll:LiteraryGreekString = LiteraryGreekString("mh")
		val quant = LongSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		val expected:String = "—"
		println(hs.toMeter)
		assert (hs.toMeter == expected)
	}

	it should "pretty print an anceps" in {
		val syll:LiteraryGreekString = LiteraryGreekString("ke")
		val quant = AncepsSyllableType
		val hs:HexameterSyllable = HexameterSyllable(syll,quant)
		val expected:String = "\u2A09"
		println(hs.toMeter)
		assert (hs.toMeter == expected)
	}
  
}
