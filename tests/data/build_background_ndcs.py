#!/usr/bin/env python3
"""Builds tests/data/background_ndcs.csv: real 11-digit NDC package codes for
common prescription drugs that are NOT in the HHS risk model's drug
categories (statins, blood pressure drugs, and so on).

tests/generate_synthetic.R uses them to give the made-up patients the
ordinary pharmacy fills a real claims table has, most of which should be
ignored by the scorer.

Source: the FDA NDC Directory through the openFDA API (https://open.fda.gov/;
public domain data, "do not rely on openFDA to make decisions regarding
medical care"). Re-run to refresh:  python3 tests/data/build_background_ndcs.py
"""
import csv, json, time, urllib.parse, urllib.request

# generic_name as the FDA lists it -> (drug class used by the generator)
DRUGS = {
    'ATORVASTATIN CALCIUM': 'statin', 'ROSUVASTATIN CALCIUM': 'statin',
    'SIMVASTATIN': 'statin', 'PRAVASTATIN SODIUM': 'statin', 'LOVASTATIN': 'statin',
    'LISINOPRIL': 'blood_pressure', 'LOSARTAN POTASSIUM': 'blood_pressure',
    'AMLODIPINE BESYLATE': 'blood_pressure', 'METOPROLOL TARTRATE': 'blood_pressure',
    'METOPROLOL SUCCINATE': 'blood_pressure', 'HYDROCHLOROTHIAZIDE': 'blood_pressure',
    'LEVOTHYROXINE SODIUM': 'thyroid',
    'OMEPRAZOLE': 'acid_reducer', 'PANTOPRAZOLE SODIUM': 'acid_reducer',
    'SERTRALINE HYDROCHLORIDE': 'antidepressant', 'ESCITALOPRAM OXALATE': 'antidepressant',
    'METFORMIN HYDROCHLORIDE': 'metformin',
    'ALBUTEROL SULFATE': 'asthma', 'MONTELUKAST SODIUM': 'asthma',
    'GABAPENTIN': 'nerve_pain',
    'AMOXICILLIN': 'antibiotic', 'AZITHROMYCIN': 'antibiotic',
    'PREDNISONE': 'steroid',
}
PER_DRUG = 150  # package codes kept per drug

def ndc11(package_ndc):
    """10-digit NDC (5-3-2, 4-4-2 or 5-4-1, hyphenated) -> 11-digit 5-4-2."""
    labeler, product, package = package_ndc.split('-')
    return labeler.zfill(5) + product.zfill(4) + package.zfill(2)

def fetch(generic):
    out = []
    for skip in (0, 100):
        q = ('generic_name:"%s" AND product_type:"HUMAN PRESCRIPTION DRUG" AND finished:true'
             % generic)
        url = 'https://api.fda.gov/drug/ndc.json?' + urllib.parse.urlencode(
            {'search': q, 'limit': 100, 'skip': skip})
        try:
            with urllib.request.urlopen(url, timeout=60) as r:
                data = json.load(r)
        except Exception as e:           # a short list is fine; a missing drug is reported below
            print('  stopped at skip=%d: %s' % (skip, e)); break
        for prod in data.get('results', []):
            if prod.get('generic_name', '').upper() != generic:
                continue
            for pk in prod.get('packaging', []):
                if not pk.get('sample'):
                    out.append(ndc11(pk['package_ndc']))
        if len(data.get('results', [])) < 100:
            break
        time.sleep(0.3)
    return sorted(set(out))[:PER_DRUG]

rows = []
for generic, cls in DRUGS.items():
    codes = fetch(generic)
    print('%-28s %-15s %3d codes' % (generic, cls, len(codes)))
    rows += [(generic.lower(), cls, c) for c in codes]

with open('tests/data/background_ndcs.csv', 'w', newline='') as f:
    w = csv.writer(f)
    w.writerow(['drug', 'drug_class', 'ndc'])
    w.writerows(rows)
print('wrote', len(rows), 'rows')
