class P1:
    def numIdenticalPairs(self, nums: List[int]) -> int:
        n = len(nums)
        ret = 0
        for i in range(n):
            for j in range(i+1, n):
                if nums[i] == nums[j]:
                    ret += 1
        return ret
class P2:
    def numSub(self, s: str) -> int:
        mod = 10**9+7
        n = len(s)
        dp = [0]*n
        for i,x in enumerate(s):
            if x == '1':
                dp[i] = (1 + dp[i-1]) % mod
        ret = 0
        for x in dp:
            ret = (ret + x) % mod
        return ret
class P3:
    def maxProbability(self, n: int, edges: List[List[int]], succProb: List[float], start: int, end: int) -> float:
        ret = 0
        graph = collections.defaultdict(list)
        for x, y in zip(edges, succProb):
            a, b = x
            graph[a].append((b,y))
            graph[b].append((a,y))


        A= [(-1.0, start)]
        vis = [False]*n
        while A:
            p,x = heapq.heappop(A)
            if vis[x]:
                continue
            vis[x] = True
            if x == end:
                return -p
            for y, py in graph[x]:
                pp = p*py
                if not vis[y]:
                    heapq.heappush(A, (pp, y))
        return 0
from math import sqrt
class P4:
    def getMinDistSum(self, A: List[List[int]]) -> float:
        ret = 0
        n = len(A)
        def f(i,j):
            d = 0
            for x,y in A:
                d += sqrt((x - i)**2 + (y - j)**2)
            return d

        P = sorted([ (i,j) for i in range(101) for j in range(101)], key=lambda x: f(x[0], x[1]))
        gx,gy = P[0]

        eps = 1e-9
        step = 100
        ret = f(gx,gy)
        print(eps)
        dx,dy = [1,-1,0,0], [0,0,1,-1]
        while step > eps:
            found = False
            for i in range(4):
                nx,ny = dx[i]*step+gx, gy+dy[i]*step
                if f(nx,ny) < ret:
                    found = True
                    ret = f(nx,ny)
                    gx,gy = nx,ny
            if not found:
                step /= 2
        return ret