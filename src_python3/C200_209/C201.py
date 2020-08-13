class P1:
    def makeGood(self, s: str) -> str:
        A = list(s)
        i = 0
        while i+1 < len(A):
            if abs(ord(A[i+1]) - ord(A[i])) == abs(ord('A') - ord('a')):
                del A[i:i+2]
                continue
            i+=1    
        ret = "".join(A)     
        return ret if ret == s else self.makeGood(ret)

class P2:
    def findKthBit(self, n: int, k: int) -> str:
        A = [0]
        for _ in range(n-1):
            A = A + [1] + [*map(lambda x: x^1, A)][::-1]
        return str(A[k-1]) 

class P3:
    def maxNonOverlapping(self, nums: [int], target: int) -> int:
        last = collections.defaultdict(int)
        last[0] = -1
        cur = ret = 0
        ep = -2
        for i,x in enumerate(nums):
            cur += x
            y = cur - target 
            if y in last and last[y] >= ep:
                ep = i
                ret += 1
            last[cur] = i    
        return ret 
class P4:
    def minCost(self, n: int, cuts: [int]) -> int:
        cuts = [0,n] + cuts
        cuts = sorted(cuts)
        A = [cuts[i+1] - cuts[i] for i in range(len(cuts)-1)]
        return self.mergeStones(A, 2)

    def mergeStones(self, stones: [int], K: int) -> int:
        N = len(stones)
        if (N - 1) % (K - 1): return -1
        prefix = [0] * (N+1)
        for i in range(1,N+1): prefix[i] = stones[i-1] + prefix[i-1]
        dp = [[0] * N for _ in range(N)]
        for m in range(K, N+1):
            for i in range(N-m+1):
                dp[i][i+m-1] = min(dp[i][k] + dp[k+1][i+m-1] for k in range(i, i+m-1, K-1)) + (prefix[i+m] - prefix[i] if (m-1)%(K-1) == 0 else 0)
        return dp[0][N-1]

if __name__ == "__main__":
    print("Build Success!")