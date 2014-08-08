package com.cbarr.ml

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.io._
import java.net.URL
import org.jsoup.nodes.Element
import collection.JavaConverters._
import math._
import FeatureFunctions.{applyFeatures,getEmptyVector,NUM_FEATURES}

object TrainAuthorModel {
  
  val DATA_FILE = "data/data.txt"
  val OUTPUT_FILE = "output/author.ser"
  val MAX_CHILDREN = 4
  val ITERATIONS = 30
  
  def main(args:Array[String]) = {
    output(getModel(getData map getFeatureVector))
  }
  
  case class model(hyperplane:Array[java.lang.Double] /** other properties of model **/)
  
  def output(authorModel:Array[Double]) = {
    val oos = new ObjectOutputStream(new FileOutputStream(OUTPUT_FILE))
    oos.writeObject(authorModel map {new java.lang.Double(_)})
    oos.flush;oos.close
  }
    
  
  /**
   * get a feature vector comprised of features in the element and its first N children
   * with a double as the label
   */
  def getFeatureVector(datum:(Element,Boolean)) = {
     (
       applyFeatures(datum._1)
        ++ { 
         val children = datum._1.children.toArray.slice(0,MAX_CHILDREN)
         (0 until MAX_CHILDREN).toArray map { i =>
           if(i < children.size) applyFeatures(children(i).asInstanceOf[Element])
           else getEmptyVector
         } reduce {_++_}
       }
      ,
      if(datum._2) 1.0 else -1.0    
    )
  }
  
  /**
   * Turn a data file of format [url	author] into a list of HTML elements paired with the author's name.
   * Throw away results whose URLs throw e.g. 403 errors.
   */
  def getData = {
    println("getting data from web")
    val data = io.Source.fromFile(DATA_FILE, "UTF-8").getLines.map{ line => {
      val cols = line.split("\\t"); (cols(0),cols(1))} }.drop(1).toArray
    data.indices.toArray.zip(data) map { case(i,(url,author)) =>
        if(i != 0 && i % 10 == 0) print(".");if(i != 0 && i % 100 == 0) print("\n")
        (author,getElements(url))
    } filter { _._2 != null} flatMap { case(author,elements) =>
      elements map{ e =>
        (e,
         e.ownText.contains(author) || e.attributes.asList.asScala.exists(_.getValue.contains(author))
         // this is my approximation of an "answer" -- ideally, there'd be labeled data
         )
      }
    }
  }
  
  def getElements(url:String) = 
    try {
      Jsoup.parse(wget(new URL(url))).getAllElements.toArray map {_.asInstanceOf[Element]}
    } catch { case e => null:Array[Element] }
  
  /**
   * Perform stochastic gradient descent
   */
  def getModel(labeledVectors:Array[(Array[Double],Double)]) = {
    val rand = new java.util.Random(42)
    var hyperplane = {(0 until (MAX_CHILDREN + 1) * NUM_FEATURES) map{ _ => 2 * rand.nextDouble - 1} toArray}
    for(iteration <- 1 to ITERATIONS){ // subtract complete gradient from hyperplane each iteration
      println("iteration "+iteration)
	  hyperplane = subtract(hyperplane, labeledVectors.map{ case(vector,label) => 
          getGradient(vector, label, hyperplane) // get partial gradient from each instance
	  }.reduce{add} ) // add them up to get complete gradient
	}
    hyperplane
  }
  
  def add(vector1:Array[Double], vector2:Array[Double]) =
    vector1 zip vector2 map { x => x._1 + x._2 }
  
  def subtract(vector1:Array[Double], vector2:Array[Double]) = 
    vector1 zip vector2 map { x => x._1 - x._2 }
  
  def dot(vector1:Array[Double], vector2:Array[Double]) =
    vector1 zip vector2 map { x => x._1 * x._2 } reduce {_+_}
    
  def multiply(vector1:Array[Double], vector2:Array[Double]) =
    vector1 zip vector2 map { x => x._1 * x._2 }
  
  def multiplyScalar(scalar:Double,vector:Array[Double]) =
    vector map {_*scalar}
  
  def getGradient(instance:Array[Double], label:Double, weights:Array[Double]) = 
    multiplyScalar((1 / (1 + exp(-label * (dot(weights,instance)))) - 1) * label , instance)
  
  def wget(url:URL) = {
    val reader = new BufferedReader(new InputStreamReader(url.openStream(), "UTF-8"))
    val sb = new StringBuilder
    var line = reader.readLine
    while(line != null){
      sb.append(line)
      line = reader.readLine
    }
    sb.toString
  }
  
}