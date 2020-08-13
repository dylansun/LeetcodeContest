import collections
class P1:
    def countGoodTriplets(self, A, a, b, c):
        n = len(A)
        def f(x,y,z):
            e = abs(x-y)
            f = abs(z-y)
            g = abs(x-z)
            return e <= a and f <= b and g <= c
        ret = 0
        for i in range(n):
            for j in range(i+1, n):
                for k in range(j+1, n):
                    if f(A[i], A[j],A[k]):
                        ret += 1
        return ret

class P2:
    def getWinner(self, A, k):
        a = A[0]
        p = 0
        for x in A[1:]:
            if x > a:
                a,p = x, 1
            else:    
                p += 1
            if p == k:    
                return a
        return a

class P3:
    def getWinner(self, A, k):
        a = A[0]
        p = 0
        for x in A[1:]:
            if x > a:
                a,p = x, 1
            else:    
                p += 1
            if p == k:    
                return a
        return a

class P4:
    def minSwaps(self, grid):
        n = len(grid)
        if not grid or n == 1: return 0
        c = collections.defaultdict(list)
        def f(A):
            ret = 0
            for x in A[::-1]:
                if not x:
                    ret += 1
                else:    
                    return ret
            return ret    
        used = [0]*n
        for i,A in enumerate(grid):
            c[f(A)].append(i)
            
        #simulation
        sel = []
        Pool = c[n] 
        ret = 0
        import bisect
        for x in range(n-1, 0, -1):
            Pool = Pool + c[x]
            Pool = [x for x in Pool if not used[x]]
            if not Pool:
                return -1
            p = min(Pool)
            used[p] = 1
            layer_id = n-1-x
            offset =len(sel)- bisect.bisect(sel, p)
            bisect.insort(sel, p)
            ret += p-layer_id+offset
        return ret

if __name__ == "__main__":
    grid = [[0,0,1],[1,1,0],[1,0,0]]
    print(P4().minSwaps(grid))