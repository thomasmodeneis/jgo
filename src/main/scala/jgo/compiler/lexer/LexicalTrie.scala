package jgo.compiler
package lexer

import scala.collection.mutable.Map
import scala.util.parsing.input.Reader

object LexicalTrie {
  //this class is mutable, but LexicalTrie's public interface prevents modification
  private[lexer] class TrieNode(val branches: Map[Char, TrieNode], var accepting: Option[String]) {
    def this() = this(Map(), None)
  }
  
  def apply(elems: String*) = {
    val ret = new LexicalTrie(new TrieNode)
    elems foreach (ret += _)
    ret
  }
}

final class LexicalTrie private (root: LexicalTrie.TrieNode) {
  import LexicalTrie.TrieNode
  
  private def += (str: String): LexicalTrie = { //Again, apologies for the imperative style.
    var cur = root                              //I'm concerned about efficiency and too
    var i = 0                                   //busy right now to deal with scalac's
    while (i < str.length) {                    //demands regarding use of @tailrec. :(
      cur = cur.branches.getOrElseUpdate(str(i), new TrieNode())
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
        return false //UGH!!
    }
    assert(if (cur.accepting.isDefined) cur.accepting.get == str else true)
    return cur.accepting.isDefined
  }
  
  def matchingPrefixOf(r: Reader[Char]): (Option[String], Reader[Char]) =
    prefixOfFromNode(root, r)
  
  private def prefixOfFromNode(cur: TrieNode, r: Reader[Char]): (Option[String], Reader[Char]) = {
    //The backtracking is nice, but I should try do do this monadically, not with if-expressions.
    //Unfortunately, (Option[A], B) is not already monadic in A, and it's not worth the effort to
    //make it so.
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
}
