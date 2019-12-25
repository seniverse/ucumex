import requests

from bs4 import BeautifulSoup as BS

METRIC_UNIT = ''
ATOM_UNIT = ''


def isMetric(raw):
    if raw == 'no':
        return False
    else:
        return True


def formatUnit(unit):
    escaped = [('[', '\\['), (']', '\\]'), ('/', '\\/'), ('(', '\\('), (')', '\\)'), ('*', '\\*'), ('+', '\\+'), ('.', '\\.'), ('^', '\\^')]

    for (s, t) in escaped:
        unit = unit.replace(s, t)


    if len(unit) == 1:
        return unit
    else:
        return '(%s)' % unit


def handleTable(table):
    global METRIC_UNIT
    global ATOM_UNIT

    rows = table.find_all('tr')
    idx = 0

    for th in rows[0].find_all('th'):
        if th.string == 'c/s':
            break
        else:
            idx += 1

    for row in rows[1:]:
        tds = row.find_all('td')
        unit = tds[idx].find('code').string

        if isMetric(tds[idx + 2].string):
            METRIC_UNIT = '%s|%s' % (METRIC_UNIT, formatUnit(unit))
        else:
            ATOM_UNIT = '%s|%s' % (ATOM_UNIT, formatUnit(unit))


if __name__ == '__main__':
    response = requests.get('http://unitsofmeasure.org/ucum.html')

    page = BS(response.content)
    tables = page.find_all('table', class_='standard')

    for table in tables[2: len(tables) - 4]:
        handleTable(table)

    print(METRIC_UNIT)
    print('\n')
    print(ATOM_UNIT)

