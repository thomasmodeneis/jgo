package jgo.compiler
package lexer

import scala.collection.mutable.Map
import scala.util.parsing.input.Reader

object LexicalTrie {
  //this class is mutable, but LexicalTrie's public interface prevents modification
  private class TrieNode {
    private[LexicalTrie] val branches:  Map[Char, TrieNode] = Map()
    private[LexicalTrie] var accepting: Option[String]      = None
  }
  
  def apply(elems: String*) = {
    val ret = new LexicalTrie
    elems foreach { ret += _ }
    ret
  }
}

final class LexicalTrie {
  import LexicalTrie.TrieNode
  
  private val root = new TrieNode
  
  private def += (str: String): LexicalTrie = {
    var cur = root
    var i = 0
    while (i < str.length) {
      cur = cur.branches.getOrElseUpdate(str(i), new TrieNode)
      i += 1
    }
    cur.accepting = Some(str)
    this
  }
  
  def contains(str: String): Boolean = {
    var cur = root
    var i = 0
    while (i < str.length) {
      if (cur.branches contains str(i)) {
        cur = cur.branches(str(i))
        i += 1
      }
      else
        return false
    }
    assert(if (cur.accepting.isDefined) cur.accepting.get == str else true)
    return cur.accepting.isDefined
  }
  
  def matchingPrefixOf(r: Reader[Char]): (Option[String], Reader[Char]) =
    prefixOfFromNode(root, r)
  
  private def prefixOfFromNode(cur: TrieNode, r: Reader[Char]): (Option[String], Reader[Char]) =
    if (cur.branches contains r.first) {
      val fromNextNode = prefixOfFromNode(cur.branches get r.first get, r.rest)
      if (fromNextNode._1.isDefined) //_1 refers to the first (there is no zeroth) term of the pair
        fromNextNode
      else
        (cur.accepting, r)
    }
    else
      (cur.accepting, r)
}
