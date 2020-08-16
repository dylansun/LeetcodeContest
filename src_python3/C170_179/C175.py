import collections
class P1:
    def countNegatives(self, grid: List[List[int]]) -> int:
        n, m = len(grid), len(grid[0])
        return sum(1 for i in range(n) for j in range(m) if grid[i][j] < 0)
class ProductOfNumbers:

    def __init__(self):
        self.A = []
        self.C = []

    def add(self, num: int) -> None:
        self.A.append(num)
        if not self.C:
            self.C.append(num)
        else:
            self.C.append(num * self.C[-1])
        if num == 0:
            self.A = []
            self.C = []

    def getProduct(self, k: int) -> int:
        if k > len(self.C): return 0
        if k == len(self.C): return self.C[-1]
        print("GetProduct", k, self.C)
        return self.C[-1] // self.C[len(self.C)-k-1]

class P3:
    def maxEvents(self, events: List[List[int]]) -> int:
        A = sorted(events, key=lambda x: (x[1], x[0]))
        cur =set([]); ret = 0; ii=1
        for s,f in A:
            for i in range(max(ii,s), f+1):
                if i not in cur:
                    cur.add(i)
                    ret += 1
                    if ii == i:
                        ii+=1
                    #print(s,f, cur, ret)
                    break
        return ret
class P4:
    def isPossible(self, A: List[int]) -> bool:
        n = len(A)
        if n == 1: return A == [1]
        s = sum(A)
        A = sorted(A)
        import bisect
        while True:
            x = A.pop()
            if x == 1: return True
            y = s - x
            if y == 1: return True
            if x < y: return False
            q = x % y
            if q == 0: return False
            bisect.insort(A, q)
            s = y + q   
if __name__ == "__main__":
    print("Hello world!")