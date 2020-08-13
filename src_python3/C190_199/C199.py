class P1:
    def restoreString(self, s, indices):
        A = zip(indices, list(s))
        A = sorted(A, key=lambda x: x[0])
        A = [v for k,v in A]
        return "".join(A)
class P2:
    def minFlips(self, target: str) -> int:
        ret, flag = 0, 0
        target = [*map(lambda x: ord(x) - ord('0'), target)]
        for x in target:
            ret += x ^ flag
            if x ^ flag: flag ^= 1
                
        return ret   

class P3:
    def countPairs(self, root: TreeNode, D: int) -> int:
        lfs = []
        if not root: return 0
        def dfs(root, P):
            if not root: return 
            P = P + [root]
            if not root.left and not root.right:
                lfs.append(P)
            if root.left:   
                dfs(root.left, P)
            if root.right:   
                dfs(root.right, P)
        ret = 0
        def dist(A, B):
            n,m=len(A), len(B)
            i = 0
            while A[i] == B[i]:
                i+= 1
            return n+m-2*i <= D 
        dfs(root, [])
        n = len(lfs)
        for p1 in range(n):
            for p2 in range(p1+1, n):
                if dist(lfs[p1], lfs[p2]):
                    ret += 1
        return ret
class P4:
    def getLengthOfOptimalCompression(self, s, k):
        def compress(ch, cnt):
            if cnt == 0:
                return ''

            if cnt == 1:
                return ch

            return '%s%d' % (ch, cnt)

        from functools import lru_cache
        @lru_cache(None)
        def dp(i, pre_ch, pre_cnt, k):
            if i == len(s):
                return len(compress(pre_ch, pre_cnt))

            candidates = []
            if k > 0:
                candidates.append(dp(i + 1, pre_ch, pre_cnt, k - 1))  # if delete s[i]

            # if keep s[i]
            if s[i] == pre_ch:
                candidates.append(dp(i + 1, pre_ch, pre_cnt + 1, k))
            else:
                candidates.append(dp(i + 1, s[i], 1, k) + len(compress(pre_ch, pre_cnt)))

            return min(candidates)

        return dp(0, '', 0, k)
if __name__ == "__main__":
    s = "cab"
    indices = [2,0,1]
    print(Solution().restoreString(s, indices))
