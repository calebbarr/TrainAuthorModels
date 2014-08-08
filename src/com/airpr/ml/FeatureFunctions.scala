package com.cbarr.ml

import java.util.regex.Pattern
import org.jsoup.nodes.Element

object FeatureFunctions {
  
  val NUM_FEATURES = 27
  
  val UNLIKELY = Pattern.compile("com(bx|ment|munity)|dis(qus|cuss)|e(xtra|[-]?mail)|foot|"
                + "header|menu|re(mark|ply)|rss|sh(are|outbox)|sponsor"
                + "a(d|ll|gegate|rchive|ttachment)|(pag(er|ination))|popup|print|"
                + "login|si(debar|gn|ngle)")
  
  val POSITIVE = Pattern.compile("(^(body|content|h?entry|main|page|post|text|blog|story|haupt))"
                + "|arti(cle|kel)|instapaper_body")
    
  val NEGATIVE = Pattern.compile("nav($|igation)|user|com(ment|bx)|(^com-)|contact|"
                + "foot|masthead|(me(dia|ta))|outbrain|promo|related|scroll|(sho(utbox|pping))|"
                + "sidebar|sponsor|tags|tool|widget|player|disclaimer|toc|infobox|vcard")
                
  val NEGATIVE_STYLE = Pattern.compile("hidden|display: ?none|font-size: ?small")
  
  val MIN_TEXT_LEN = 20
  val MAX_TEXT_LEN = 200
  
  
  def applyFeatures(e:Element) = 
    (0 until NUM_FEATURES).toArray.map {i => applyFeature(e,i)} 
  
  def applyFeature(e:Element,i:Int) = i match {
    case 0 => classPositive(e)
    case 1 => classNegative(e)
    case 2 => classUnlikely(e)
    case 3 => idPositive(e)
    case 4 => idNegative(e)
    case 5 => idUnlikely(e)
    case 6 => negativeStyle(e)
    case 7 => textLessThanMin(e)
    case 8 => textMoreThanMax(e)
    case 9 => h1Tag(e)
    case 10 => h2Tag(e) 
    case 11 => h3Tag(e)
    case 12 => h4Tag(e)
    case 13 => h5Tag(e)
    case 14 => h6Tag(e)
    case 15 => divTag(e)
    case 16 => tableTag(e)
    case 17 => liTag(e)
    case 18 => tdTag(e)
    case 19 => thTag(e)
    case 20 => pTag(e)
    case 21 => textLength(e)
    case 22 => caption(e)
    case 23 => countLt(e)
    case 24 => countGt(e)
    case 25 => countPx(e)
    case 26 => ltGtPxThreshold(e)
  }
  
  def getEmptyVector =
    (0 until NUM_FEATURES) map { _ => 0.0} toArray
  
                
  //0
  def classPositive(e:Element) =
    if (POSITIVE.matcher(e.className).find) 1.0 else 0.0
  
  //1
  def classNegative(e:Element) =
    if (NEGATIVE.matcher(e.className).find) 1.0 else 0.0
    
  //2
  def classUnlikely(e:Element) =
    if (UNLIKELY.matcher(e.className).find) 1.0 else 0.0
  
  //3
  def idPositive(e:Element) =
    if (POSITIVE.matcher(e.id).find) 1.0 else 0.0
  
  //4  
  def idNegative(e:Element) =
    if (NEGATIVE.matcher(e.id).find) 1.0 else 0.0
  
  //5
  def idUnlikely(e:Element) =
    if (UNLIKELY.matcher(e.id).find) 1.0 else 0.0
  
  //6
  def negativeStyle(e:Element) = {
    val style = e.attr("style")
    if (style != null && !style.isEmpty && NEGATIVE_STYLE.matcher(style).find) 1.0 else 0.0  
  }
  
  //7
  def textLessThanMin(e:Element) =
    if(e.ownText.size < MIN_TEXT_LEN) 1.0 else 0.0
    
  //8
  def textMoreThanMax(e:Element) =
    if(e.ownText.size < MAX_TEXT_LEN) 1.0 else 0.0
  
  //9
  def h1Tag(e:Element) =
    if(e.tagName == "h1") 1.0 else 0.0
      
  //10
  def h2Tag(e:Element) =
    if(e.tagName == "h2") 1.0 else 0.0
      
  //11
  def h3Tag(e:Element) =
    if(e.tagName == "h3") 1.0 else 0.0
      
  //12
  def h4Tag(e:Element) =
    if(e.tagName == "h4") 1.0 else 0.0
      
  //13
  def h5Tag(e:Element) =
    if(e.tagName == "h5") 1.0 else 0.0
      
  //14
  def h6Tag(e:Element) =
    if(e.tagName == "h6") 1.0 else 0.0
  
  //15
  def divTag(e:Element) =
    if(e.tagName == "div") 1.0 else 0.0
  
  //16
  def tableTag(e:Element) =
    if(e.tagName == "table") 1.0 else 0.0

  //17
  def liTag(e:Element) =
    if(e.tagName == "li") 1.0 else 0.0
      
  //18
  def tdTag(e:Element) =
    if(e.tagName == "td") 1.0 else 0.0
      
  //19
  def thTag(e:Element) =
    if(e.tagName == "th") 1.0 else 0.0
      
  //20
  def pTag(e:Element) =
    if(e.tagName == "p") 1.0 else 0.0
  
  //21
  def textLength(e:Element) =
    e.ownText.size.toDouble
  
  //22
  def caption(e:Element) =
    if(e.className.toLowerCase == "caption") 1.0 else 0.0
  
  //23
  def countLt(e:Element) =
    countSubstring(e.ownText,"&lt;").toDouble
  
  //24
  def countGt(e:Element) =
    countSubstring(e.ownText,"&gt;").toDouble
    
  //25
  def countPx(e:Element) =
    countSubstring(e.ownText, "px").toDouble
  
  //26
  def ltGtPxThreshold(e:Element) =
    if(countLt(e) + countGt(e) + countPx(e) > 5.0) 1.0 else 0.0
  
  def countSubstring(str1:String, str2:String):Int={
    import scala.annotation.tailrec
    @tailrec def count(pos:Int, c:Int):Int={
      val idx=str1 indexOf(str2, pos)
      if(idx == -1) c else count(idx+str2.size, c+1)
    }
    count(0,0)
  }
  
}