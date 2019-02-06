package edu.holycross.shot.greek

import scala.scalajs.js
import scala.scalajs.js.annotation._


/** A quantified syllable of Greek hexameter
*
* @param lgs a LiteraryGreekString object
* @param quantity a SyllableQuantityType
*/
@JSExportAll  case class HexameterSyllable(lgs: LiteraryGreekString, quantity:SyllableQuantityType) {

    override def toString = {
      quantity match {
        case LongSyllableType => s"${lgs.ucode}\u0305"
        case ShortSyllableType => s"${lgs.ucode}\u0306"
        case _ => s"${lgs.ucode}\u033D"
      }
    }

    def toMeter = {
      quantity match {
        case LongSyllableType => s"—"
        case ShortSyllableType => s"\u23D1"
        case _ => s"\u2A09"
      }
    }
}


@JSExportAll object HexameterParser {

  def munge(lgs:LiteraryGreekString):String = {
    val s:String = lgs.toLower.stripAccent.ascii
    s.replaceAll("""[ ,;:".']""","").replaceAll("c","ks").replaceAll("y","ps")
  }


  /*
  Routines to split strings follow
  */

  def splitOnDiaeresis(s:LiteraryGreekString): Vector[LiteraryGreekString] = {
    splitOnDiaeresis(Vector(s))
  }

  def splitOnDiaeresis(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val mungeVec:Vector[String] = v.map(lgs => munge(lgs))
    val diaPattern = "(.*[aeiouhw][\\)\\(]?)([iu][\\)\\(]?\\+)(.*)".r
    mungeVec.flatMap (gs => {
      gs match {
        case diaPattern(lead, vwl, trail) => {
          val matchingContent = Vector(lead,vwl,trail).filter(_.nonEmpty)
          val gsVector:Vector[LiteraryGreekString] = matchingContent.map(LiteraryGreekString(_))
          splitOnDiaeresis(gsVector)
        }
        case _ => Vector(LiteraryGreekString(gs))
      }
    })
  }

  def splitOnMuNu(s:LiteraryGreekString): Vector[LiteraryGreekString] = {
    splitOnMuNu(Vector(s))
  }

  def splitOnMuNu(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val mungeVec:Vector[String] = v.map(lgs => munge(lgs))
    val mnPattern = "(.*[aeiouhw\\|\\+])mn(.*)".r
    mungeVec.flatMap (gs => {
      gs match {
        case mnPattern(leadCluster,trail) => {
          val matchingContent = Vector(leadCluster,"mn" + trail).filter(_.nonEmpty)
          val gsVector:Vector[LiteraryGreekString] = matchingContent.map(LiteraryGreekString(_))
          splitOnMuNu(gsVector)
        }

        case _ => Vector(LiteraryGreekString(gs))
        }
      })
    }

    def splitOnLiqCons(s:LiteraryGreekString): Vector[LiteraryGreekString] = {
      splitOnLiqCons(Vector(s))
    }

    def splitOnLiqCons(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
      val mungeVec:Vector[String] = v.map(lgs => munge(lgs))
      val liqConsPattern = "(.+)([lmnr])([bgdzqkcprstfxy]+.*)".r
      mungeVec.flatMap { gs => {
        gs match {
          case liqConsPattern(leadCluster,liquid,trail) => {
            val matchingContent = Vector(leadCluster + liquid, trail).filter(_.nonEmpty)
            val gsVector = matchingContent.map(LiteraryGreekString(_))
            splitOnLiqCons(gsVector)
          }
        case _ => Vector(LiteraryGreekString(gs))
        }
      }
    }
  }

 def splitOnDiphthVowel(s:LiteraryGreekString): Vector[LiteraryGreekString] = {
      splitOnDiphthVowel(Vector(s))
  }

  def splitOnDiphthVowel(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val mungeVec:Vector[String] = v.map(lgs => munge(lgs))
    val diphthVowelPattern = "(.*)(ai[\\)\\(=\\/]*|oi[\\)\\(=\\/]*|ei[\\)\\(=\\/]*|au[\\)\\(=\\/]*|eu[\\)\\(=\\/]*|ou[\\)\\(=\\/]*|hu[\\)\\(=\\/]*|wu[\\)\\(=\\/]*|ui[\\)\\(=\\/]*)([aeiouhw].*)".r
    mungeVec.flatMap (gs => {
      gs match {
        case diphthVowelPattern(lead, vwl, trail) => {
          val matchingContent = Vector(lead + vwl,trail).filter(_.nonEmpty)
          val gsVector = matchingContent.map(LiteraryGreekString(_))
          splitOnDiphthVowel(gsVector)
        }
        case _ => Vector(LiteraryGreekString(gs))
      }
    })
  }

  def splitOnVowelDiphth(s:LiteraryGreekString): Vector[LiteraryGreekString] = {
      splitOnVowelDiphth(Vector(s))
  }

  def splitOnVowelDiphth(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val mungeVec:Vector[String] = v.map(lgs => munge(lgs))
    val vowelDiphPattern = "(.*[aeiouhw][\\(\\)/=]*)(ai|oi|ei|au|eu|ou|hu|wu|ui)(.*)".r

     mungeVec.flatMap (gs => {
     gs match {
          case vowelDiphPattern(lead, diph, trail) => {
            if (trail.isEmpty) {
              val matchVector = Vector(lead,diph).map(LiteraryGreekString(_))
              splitOnVowelDiphth(matchVector)
            } else {
              val matchVector = Vector(lead,diph + trail).map(LiteraryGreekString(_))
              splitOnVowelDiphth(matchVector)
            }
        }
        case _ => Vector(LiteraryGreekString(gs))
      }
    })
  }

  def syllabify(s:LiteraryGreekString):Vector[LiteraryGreekString] = {
    syllabify(Vector(s))
  }

  def syllabify(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val dia = splitOnDiaeresis(v)
    val mn = splitOnMuNu(dia)
    val lc = splitOnLiqCons(mn)
    val dv = splitOnDiphthVowel(lc)
    val vd = splitOnVowelDiphth(dv)
    vd
  }
}


