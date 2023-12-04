from collections import deque


def get_points(line: str):
    line = line.split(":")[1].strip()
    tmp = line.split("|")
    winning = {int(x) for x in tmp[0].strip().split(" ") if len(x) > 0}
    mine = [int(x) for x in tmp[1].strip().split(" ") if len(x) > 0]
    matches = 0
    for num in mine:
        if num in winning:
            matches += 1

    return 0 if matches == 0 else 1 << (matches - 1), matches


def solve(lines):
    points_sum = 0
    multipliers = [1] * len(lines)
    cq = deque()
    for idx, line in enumerate(lines):
        cq.append(idx)
        points, matches = get_points(line)
        for won_idx in range(idx + 1, idx + 1 + matches):
            multipliers[won_idx] += multipliers[idx]
        points_sum += points
    print(f"{points_sum=}")
    total_cards = sum(multipliers)
    print(f"{total_cards=}")


def main():
    with open("inputs/day4.txt") as f:
        lines = f.readlines()
        solve(lines)


if __name__ == "__main__":
    main()
