class P1:
    def reformatDate(self, date: str) -> str:
        B = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
        day, mon, year = date.split()
        def f(x):
            if x < 10: return "0"+str(x) 
            else: return str(x)
        def g(x):
            T = ""
            for i in x:
                if '0'<=i<='9':
                    T = T+i
                else:
                    break
            return int(T)
        def g2(x):
            for i,a in enumerate(B):
                if a == x: return f(i+1)
            return ""
        return year + "-"+g2(mon)+"-"+f(g(day))
class P2:
    def rangeSum(self, nums: [int], n: int, left: int, right: int) -> int:
        A= []
        for i in range(n):
            for j in range(i,n):
                A.append(sum(nums[i:j+1]))
        A = sorted(A)
        return sum(A[left-1:right])
class P3:
    def minDifference(self, A: [int]) -> int:
        if len(A) <= 4: return 0
        A = sorted(A)  
        return min([A[-4+i]-A[i] for i in range(4)])

from functools import lru_cache
class P4:
    def winnerSquareGame(self, n: int) -> bool:
        i = 1
        A= []
        while i*i <= n:
            A.append(i*i)
            i+=1
            
        @lru_cache(None)
        def dp(x):
            if x == 0: 
                return False
            for a in A:
                if a > x:
                    break
                if not dp(x-a):
                    return True
            return False
        
        d = [False]*(n+1)
        for x in range(1, n+1):
            for y in A:
                if y <= x:
                    d[x] = d[x] or (not d[x-y])
        return d[n]        

if __name__ == "__main__":
    print("Build Success!")