package com.tibo

import scala.collection.immutable.IndexedSeq
import scala.io.{BufferedSource, Source}

/**
  * Created by tibo.delor on 7/01/2016.
  */
object SudokuParser {

  def parse(file:BufferedSource): Seq[Seq[Option[Int]]] ={
    file.getLines().toSeq.map(_.replace("|","").replace(" ","").map(c => {
      if(c=='.') None
      else if(c.isDigit && c!='0') Some(c.asDigit)
      else throw new RuntimeException(s"INvalid output : $c")
    }))
  }


}
