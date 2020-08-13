

class P3:
    def maxNumOfSubstrings(self, s: str) -> [str]:
        n = len(s)
        A = [[n, -1] for _ in range(26)]
        for i, x in enumerate(s):
            x = ord(x) - ord('a')
            A[x][0] = min(A[x][0], i)
            A[x][1] = max(A[x][1], i)

        cand = []
        for i in range(26):
            l, r = A[i]
            if r == -1:
                continue
            valid = True
            print("*"*10, "\n", A[i])
            print("i",i)
            i_ch = l+1
            while i_ch < r:
                j = ord(s[i_ch]) - ord('a')
                l2, r2 = A[j]
                if i == 2:
                    print(s[i_ch], l2, r2)
                if l2 < l:
                    valid = False
                    break
                r = max(r, r2)
                i_ch += 1
            if valid:
                print("valid", i, l, r)
                cand.append([l, r])
        cand = sorted(cand, key=lambda x: x[1] - x[0])
        print(cand)
        ret = []
        for x in cand:
            flag = True
            a, b = x
            for y in ret:
                c, d = y
                if a < c < b or a < d < b:
                    flag = False
                    break
            if flag:
                ret.append(x)

        return [s[a:b + 1] for a, b in ret]
'''
s2 = "bbcacbaba"
ans = P3().maxNumOfSubstrings(s2)
print(ans)
'''
class P4:
    def closestToTarget(self, arr:[int], target: int) -> int:
        s = set([])
        ret = 10**7
        for x in arr:
            s2 = set([])
            ret = min(ret, abs(x-target))
            if ret == 0:
                return 0
            if x > target:
                s2.add(x)
            for y in s:
                if (x & y) >= target:
                    ret = min(ret, abs(target -(x&y)))
                    s2.add(x&y)
            s = s2
        return ret