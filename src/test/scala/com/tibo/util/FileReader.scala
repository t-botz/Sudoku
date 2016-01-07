package com.tibo.util

import scala.io.{BufferedSource, Source}

/**
  * Created by tibo.delor on 7/01/2016.
  */
object FileReader {

  def read(fileName:String): BufferedSource = {
    Source.fromInputStream(getClass.getResourceAsStream(fileName))
  }
}
