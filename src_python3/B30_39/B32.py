import collections
class P1:
    def findKthPositive(self, arr: [int], k: int) -> int:
        s = set(arr)
        A = [x for x in range(1, 2001) if x not in s]
        return A[k-1]
class P2:
    def canConvertString(self, s: str, t: str, K: int) -> bool:
        if len(s) != len(t):
            return False
        def dist(a, b):
            delta = ord(b) - ord(a)
            delta = (delta + 26) % 26
            return delta
        d = collections.defaultdict(int)
        for a,b in zip(s,t):
            di = dist(a, b)
            d[di] += 1
        ret = 0    
        for k,v in d.items():
            if v > 0 and k!=0:
                ret = max(ret, k + 26*(v-1))
        print(s,t,k, ret)    
        return ret <= K    
class P3:
    def minInsertions(self, s: str) -> int:
        ret = 0
        L = R = 0
        for i,x in enumerate(s):
            #print("*"*10)
            #print(i,x, L,R, ret)
            if x == '(':
                if R == 1:
                    if L>0:
                        L -= 1
                        R = 0
                        ret += 1
                L += 1
            if x == ')':        
                if L == 0:
                    L += 1
                    ret += 1
                if R ==0:    
                    R += 1
                else:    
                    R = 0
                    L -= 1
            #print(i,x, L,R, ret)
        ret += 2*L - R
        return ret
class P4:
    def longestAwesome(self, s: str) -> int:
        n = len(s)
        dp = [n] * (1 << 10)
        dp[0] = -1
        pre = 0
        ret = 0
        for i,ch in enumerate(s):
            x = ord(ch) - ord('0')
            pre = pre + ( 1<< x) if pre & (1 << x) == 0 else pre - (1 << x)
            ret = max(ret, i - dp[pre])
            for k in range(10):    
                if pre & (1 << k) == 0:
                    ret = max(ret, i-dp[pre + (1 << k)])
                else:    
                    ret = max(ret, i-dp[pre - (1 << k)])
            dp[pre] = min(dp[pre],i)
        return ret
                