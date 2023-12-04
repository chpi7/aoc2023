from collections import defaultdict

""" Maps gear index tuple to the list of adjacent numbers:
    (row_idx, char_idx) -> list[int]"""
gears = defaultdict(list)


def find_numbers(line: str):
    start_idx = 0
    current = ""
    last_digit = False
    for idx, char in enumerate(line):
        if last_digit and char.isdigit():
            current += char
        elif not last_digit and char.isdigit():
            current = char
            start_idx = idx
        elif last_digit and not char.isdigit():
            yield current, start_idx
            current = ""
            start_idx = 0
        else:
            # jump over whatever is here
            pass

        last_digit = char.isdigit()

    if current != "":
        yield current, start_idx


def is_adjacent(number: str, idx: int, lines: list[str], lineidx: int):
    global gears

    for row_idx in [lineidx - 1, lineidx + 1]:
        for char_idx in range(idx - 1, idx + len(number) + 1):
            char = lines[row_idx][char_idx]
            if not (char.isdigit() or char == "."):
                if char == "*":
                    # found gear
                    gears[(row_idx, char_idx)].append(int(number))
                return True

    pred_char = lines[lineidx][idx - 1]
    succ_char = lines[lineidx][idx + len(number)]
    if not (pred_char == "." or pred_char.isdigit()):
        row_idx = lineidx
        char_idx = idx - 1
        if pred_char == "*":
            # found gear
            gears[(row_idx, char_idx)].append(int(number))
        return True
    elif not (succ_char == "." or succ_char.isdigit()):
        row_idx = lineidx
        char_idx = idx + len(number)
        if succ_char == "*":
            # found gear
            gears[(row_idx, char_idx)].append(int(number))
        return True

    return False


def compute_gear_ratio_sum():
    sum = 0
    for nums in gears.values():
        if len(nums) == 2:
            sum += nums[0] * nums[1]
    return sum


def pad_lines(lines: list[str]) -> list[str]:
    linelen = len(lines[0]) + 2
    padded_orig = ["." + line + "." for line in lines]
    first_last = "." * linelen
    return [first_last] + padded_orig + [first_last]


def strip_lines(lines: list[str]) -> list[str]:
    return [line.strip() for line in lines]


def solve(lines: list[str]):
    lines = strip_lines(lines)
    lines = pad_lines(lines)
    sum = 0
    for lineidx, line in enumerate(lines):
        for number, idx in find_numbers(line):
            if is_adjacent(number, idx, lines, lineidx):
                sum += int(number)
    print(f"{sum=}")
    gear_ratio_sum = compute_gear_ratio_sum()
    print(f"{gear_ratio_sum=}")


def main():
    with open("inputs/day3.txt") as f:
        lines = f.readlines()
    solve(lines)


if __name__ == "__main__":
    main()
