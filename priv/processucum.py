import xmltodict

if __name__ == '__main__':
    with open('ucum-essence.xml', 'r') as ucumRawXML:
        unitXML = xmltodict.parse(ucumRawXML.read())

        for unitDict in unitXML['root']['unit']:
            if '@isSpecial' in unitDict and unitDict['@isSpecial']:
                unit = unitDict['@Code']
                print('  defp to_basic_units("%s"), do: raise NotImplementedError' % unit)
            else:
                unit = unitDict['@Code']
                valueUnit = unitDict['value']['@Unit']
                factor = unitDict['value']['@value']

                print('  defp to_basic_units("%s"), do: {&Kernel.*/2, %s, "%s"}' % (unit, factor, valueUnit))

        print(' defp to_basic_units(_unit), do: {&Kernel.*/2, 1, "1"}')
