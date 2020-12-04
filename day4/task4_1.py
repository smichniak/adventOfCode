from typing import List


def isValidPassport(passport: str, required_fields: List[str]) -> bool:
    passport = passport.replace('\n', ':').replace(' ', ':').split(':')
    passport = passport[::2]
    for field in required_fields:
        if field not in passport:
            return False
    return True


def getPassports(intput_file: str) -> List[str]:
    with open(intput_file) as file:
        f = file.read()
        passports = f.split('\n\n')
    return passports


def main() -> None:
    REQUIRED_FIELDS = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']
    passports = getPassports('4.in')
    valid = 0
    for passports in passports:
        if isValidPassport(passports, REQUIRED_FIELDS):
            valid += 1
    print(valid)

# main()
