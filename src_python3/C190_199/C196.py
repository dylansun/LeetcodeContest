import collections
import bisect


class P1:
    def canMakeArithmeticProgression(self, A: [int]) -> bool:
        A = sorted(A)
        d = A[1]-A[0]
        for i in range(1, len(A)-1):
            if A[i+1]-A[i]!=d:
                return False
        return True


class P2:
    def getLastMoment(self, n: int, left: [int], right: [int]) -> int:
        return max(left + [*map(lambda x: n-x, right)])


class P3:
    def numSubmat(self, arr: [[int]]) -> int:
        row, col = len(arr), len(arr[0])
        p_arr = [[0] * (col + 1) for _ in range(row + 1)]
        for i in range(0, row):
            for j in range(col - 1, -1, -1):
                if not arr[i][j]:
                    continue
                if j != col - 1: p_arr[i][j] += p_arr[i][j + 1]
                p_arr[i][j] += arr[i][j]
        ans = 0

        def f(A):
            if not A or not any(A): return 0
            B, cnt, ret = [], 0, 0
            for x in A:
                if x == 0:
                    cnt = 0
                    if B and B[-1] != 0:
                        B.append(x)
                else:
                    cnt += 1
                    ret += cnt
                    B.append(x - 1)
            return ret + f(B)

        for j in range(0, col):
            ans += f([p_arr[i][j] for i in range(row)])
        return ans


class P4:
    def minInteger(self, num: str, k: int, debug=False) -> str:
        n = len(num)
        if k >= (n - 1) * n // 2: return "".join(map(str, sorted(num)))
        if k == 0: return num

        c = collections.defaultdict(list)
        num = [*map(int, list(num))]

        def swap(i, j):
            num[i], num[i + 1:j + 1] = num[j], num[i:j]

        for i, x in enumerate(num):
            c[x].append(i)

        A = []
        if debug:
            print(num)
        for i in range(n):
            if debug:
                print("-" * 10)
                print(i, "\n", c, "\n", A, "\n", num)
                print("x", num[i])
            found = False
            x = num[i]
            for d in range(0, x):
                if d not in c or not c[d]: continue
                pos = len(A) - bisect.bisect(A, c[d][0])
                pos_x = len(A) - bisect.bisect(A, c[x][0])
                if debug:
                    print(i, d, pos)
                if k >= c[d][0] + pos - c[x][0] - pos_x:
                    if debug:
                        print("found", d, "shift", pos, "need", c[d][0] + pos - c[x][0] - pos_x, "remain", k)
                    index_d = c[d][0]
                    k -= c[d][0] + pos - c[x][0] - pos_x
                    if debug:
                        print("before swap", i, c[x][0], pos_x)
                    swap(i, index_d + pos)
                    bisect.insort(A, c[d][0])
                    found = True
                    del c[d][0]
                else:
                    if debug:
                        print("i", i, "no enough swap for ", d)

                if found:
                    break
            if not found:
                del c[x][0]
            if k == 0:
                break
        return "".join(map(str, num))

    def minInteger_bf(self, num: str, k: int) -> str:
        if num == '': return ''
        if k == 0: return num
        for i in range(10):
            ind = num.find(str(i))
            if 0 <= ind <= k: return str(num[ind]) + self.minInteger(num[0:ind] + num[ind + 1:], k - ind)


def mega_test():
    import random
    passed = 0
    times = 100
    cases = []
    for _ in range(times):
        A = []
        for x in range(400):
            A.append(random.randint(0, 9))
        num = "".join(map(str, A))
        ret = P4().minInteger(num, 40, False)
        ans = P4().minInteger_bf(num, 40)
        if ret == ans:
            passed += 1
        else:
            cases.append(num)
    print("passed", passed, "total", times)
    return cases


def single_test():
    num = "2040"
    ret = P4().minInteger(num, 4, True)
    ans = P4().minInteger_bf(num, 4)
    print(ret, ans)


if __name__ == '__main__':
    single = False
    if not single:
        print(mega_test())
    else:
        single_test()
