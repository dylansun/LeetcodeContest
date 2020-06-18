import scala.collection.mutable
object C193 {
	object P1{
		def runningSum(nums: Array[Int]): Array[Int] = {
			val ret = Array.fill(nums.length)(0)
			ret.indices foreach {i => if(i==0) ret(i) = nums(i) else ret(i) = ret(i-1) + nums(i)}
			ret
		}
	}
  object P2{
    def findLeastNumOfUniqueInts(arr: Array[Int], K: Int): Int = {
      val m = mutable.Map.empty[Int, Int]
      arr foreach {x => if(m.contains(x)) m(x) += 1 else m.put(x, 1)}
			var ret = m.size
			var k = K
			m.values.toList.sorted foreach { n =>
				if(n == k) return ret -1
				else if(n < k) {k -= n; ret -=1 }
				else return ret
			}
      0
    }
  }
	object P3{
		def minDays(bloomDay: Array[Int], m: Int, k: Int): Int = {
			def check(t:Int):Boolean = {
				var cur = 0 ;var cur_m = 0
				for {x <- bloomDay}{
					if(x > t) cur = 0 else cur += 1
					if(cur == k ) {cur = 0; cur_m += 1}
				}
				cur_m >= m
			}

			def bs(l:Int, r:Int):Int = {
				if(l == r) if(check(l)) l else -1
				else if (l > r) -1
				else {
					val mid = (l + r) / 2
					if(check(mid)) bs(l, mid)
					else bs(mid+1, r)
				}
			}

			bs(1, 1e9.toInt)
		}
	}
	object P4{
		class TreeAncestor(_n: Int, _parent: Array[Int]) {
			val parent = Array.fill(20, _n)(-1)
			_parent.indices foreach { i => parent(0)(i) = _parent(i) }
			for {k <- 1 to 20; i <- 0 until _n}
				if (parent(k - 1)(i) != -1)
					parent(k)(i) = parent(k - 1)(parent(k - 1)(i))

			def getKthAncestor(node: Int, K: Int): Int = {
				var ret = node
				for {k <- 0 to 20 if ((1 << k) & K) != 0}
					if (ret != -1) ret = parent(k)(ret)
				ret
			}
		}
	}
}
