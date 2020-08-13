class P1:
    def countOdds(self, low: int, high: int) -> int:
        n = high - low +1
        if low % 2==0:
            return n//2
        return n//2 if n%2==0 else n//2 + 1   

class P2:
    def numOfSubarrays(self, arr: [int]) -> int:
        n = len(arr)
        mod = 10**9 + 7
        dp = [[0,0] for _ in range(n)]
        for i in range(n):
            x = arr[i]
            if x % 2 == 0:
                dp[i][0] = dp[i-1][0] + 1
                dp[i][1] = dp[i-1][1]
            else:   
                dp[i][0] = dp[i-1][1] 
                dp[i][1] = dp[i-1][0] + 1
                
        ret = 0         
        for i in range(n): 
            ret = (ret + dp[i][1]) % mod
        return ret    
class P3:
    def numSplits(self, s: str) -> int:
        A = [0] * 26
        B = [0] * 26    
        l, r, ret = 0, 0, 0
        for x in s:
            i = ord(x) - ord('a') 
            A[i] += 1
            r += 1 if A[i] == 1 else 0
        for x in s:
            i = ord(x) - ord('a') 
            B[i] += 1
            l += 1 if B[i] == 1 else 0
            r -= 1 if B[i] == A[i] and B[i] > 0 else 0
            ret += l == r
        return ret   
class P4:
    def minNumberOperations(self, A: [int]) -> int:
        ret, n = A[0], len(A)
        for i in range(1,n):
            ret += max(0, A[i] - A[i-1])
        return ret   