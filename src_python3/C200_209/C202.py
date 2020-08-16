
from functools import lru_cache
import bisect
class P1:
    def threeConsecutiveOdds(self, arr: [int]) -> bool:
        cur = 0
        for x in arr:
            cur = (x & 1) * (cur + (x & 1))
            if cur == 3: 
                return True
        return False 
class P2:
    def minOperations_2(self, n: int) -> int:
        m = (n & 1) ^ 1
        return (n - 1 + m) * ((n-1 - m)//2 + 1) // 2
    def minOperations(self, n: int) -> int:
        A = [2*i + 1 for i in range(n)]
        avg = sum(A) // len(A)
        return sum(abs(x - avg) for x in A) // 2
    def minOperations_3(self, n: int) -> int:
        return n**2 // 4
class P3:
    def maxDistance(self, A:[int], m: int) -> int:
        A= sorted(A)
        def check(t):
            c, cnt = A[0], 1 
            for x in A[1:]:
                if x -c >= t:
                    cnt += 1
                    c = x
                if cnt >= m:   
                    return True
            return False    
            
        l, r = 1, A[-1] - A[0]
        while l < r:
            mid = (l + r) >> 1
            if check(mid):
                if check(mid+1):
                    l = mid+1
                else:    
                    return mid
            else:   
                r = mid -1
        return  l       

class P4:
    def minDays(self, n: int) -> int:
        @lru_cache(None)
        def dp(x):
            if x == 0: return 0
            if x == 1: return 1
            return min(dp(x//2) + (x%2) + 1, dp(x//3)+(x%3) + 1)
        return dp(n)
if __name__ == "__main__":
    print(P4().minDays(1000))
