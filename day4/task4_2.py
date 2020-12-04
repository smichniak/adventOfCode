from day4.task4_1 import getPassports, List


def validYear(year: str, minimum: int, maximum: int) -> bool:
    if not (len(year) == 4 and year.isnumeric()):
        return False
    return minimum <= int(year) <= maximum


def validHeight(height: str) -> bool:
    if len(height) < 4 or len(height) > 5:
        return False
    height_type = height[-2:]
    if height_type == 'cm':
        return 150 <= int(height[:-2]) <= 193
    elif height_type == 'in':
        return 59 <= int(height[:-2]) <= 76
    else:
        return False


def validHairColor(color: str) -> bool:
    if color[0] != '#' or len(color) != 7:
        return False
    rest = color[1:]
    for char in rest:
        if not (char.isnumeric() or 'a' <= char <= 'f'):
            return False
    return True


def validEyeColor(color: str) -> bool:
    EYE_COLORS = ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']
    return color in EYE_COLORS


def validPassportID(pid: str) -> bool:
    return pid.isnumeric() and len(pid) == 9


def validField(field: str, value: str) -> bool:
    if field == 'byr':
        return validYear(value, 1920, 2002)
    elif field == 'iyr':
        return validYear(value, 2010, 2020)
    elif field == 'eyr':
        return validYear(value, 2020, 2030)
    elif field == 'hgt':
        return validHeight(value)
    elif field == 'hcl':
        return validHairColor(value)
    elif field == 'ecl':
        return validEyeColor(value)
    elif field == 'pid':
        return validPassportID(value)
    else:
        return field == 'cid'


def allFieldsPresent(passport: List[str]) -> bool:
    REQUIRED_FIELDS = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']
    passport_fields = passport[::2]
    for field in REQUIRED_FIELDS:
        if field not in passport_fields:
            return False
    return True


def validFields(passport: List[str]) -> bool:
    for i in range(0, len(passport), 2):
        if not validField(passport[i], passport[i + 1]):
            return False
    return True


def isValidPassport(passport: str) -> bool:
    passport = passport.replace('\n', ':').replace(' ', ':').split(':')
    return allFieldsPresent(passport) and validFields(passport)


def main() -> None:
    passports = getPassports('4.in')
    valid = 0
    for passports in passports:
        valid += isValidPassport(passports)
    print(valid)


main()
