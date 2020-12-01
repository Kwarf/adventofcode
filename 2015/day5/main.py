import argparse
import re


class StringRuleViolation(Exception):
    pass


def count_part_1_nice_strings(strings):
    rules = []
    rules.append(lambda string: len(re.findall(r'[aeiou]', string)) >= 3)
    rules.append(lambda string: re.search(r'(\w)\1+', string) is not None)
    rules.append(lambda string: re.search(r'ab|cd|pq|xy', string) is None)

    count = 0
    for string in strings:
        try:
            for rule_check in rules:
                if not rule_check(string):
                    raise StringRuleViolation
        except StringRuleViolation:
            continue

        count += 1
    return count


def count_part_2_nice_strings(strings):
    rules = []
    rules.append(lambda string: re.search(r'(\w\w).*\1', string) is not None)
    rules.append(lambda string: re.search(r'(\w)[^\1]\1', string) is not None)

    count = 0
    for string in strings:
        try:
            for rule_check in rules:
                if not rule_check(string):
                    raise StringRuleViolation
        except StringRuleViolation:
            continue

        count += 1
    return count


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Count nice strings')
    parser.add_argument('file', help='Input file')
    args = parser.parse_args()

    with open(args.file) as file:
        content = file.readlines()

        print('Part 1: '
              + str(count_part_1_nice_strings(content))
              + ' nice strings')

        print('Part 2: '
              + str(count_part_2_nice_strings(content))
              + ' nice strings')
