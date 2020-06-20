"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Hist, Pmf, Suite, MakeMixture


def test_vote():
    #

    def add(pmf1, pmf2):
        res = Pmf()
        for v1, p1 in pmf1.Items():
            for v2, p2 in pmf2.Items():
                res[v1, v2] = p1 * p2
        return res

    #

    from sympy import symbols

    p_citizen, p_cv, p_ncv, p_error = symbols("p_citizen, p_cv, p_ncv, p_error")

    #

    def make_binary(p, name1, name2):
        return Pmf({name1: p, name2: 1 - p})

    #

    citizen_status = ["citizen", "non-citizen"]
    pmf_citizen = make_binary(p_citizen, *citizen_status)

    #

    error_status = ["error", "no-error"]
    pmf_error = make_binary(p_error, *error_status)

    #

    pmf_citizen_report = add(pmf_citizen, pmf_error)
    pmf_citizen_report.Print()

    #

    vote_status = ["vote", "no-vote"]
    pmf_cv = make_binary(p_cv, *vote_status)

    #

    pmf_cv_report = add(pmf_cv, pmf_error)
    pmf_cv_report.Print()

    #

    pmf_ncv = make_binary(p_ncv, *vote_status)

    #

    pmf_ncv_report = add(pmf_ncv, pmf_error)
    pmf_ncv_report.Print()

    #

    mix = Pmf()

    for val1, p1 in pmf_citizen_report.Items():
        c, e = val1
        pmf = pmf_cv_report if c == "citizen" else pmf_ncv_report
        for val2, p2 in pmf.Items():
            mix[val1, val2] = p1 * p2

    mix.Print()

    #

    def report(state, alternatives):
        val, error = state
        if error != "error":
            return val
        alt1, alt2 = alternatives
        return alt1 if val == alt2 else alt2

    #

    report(("citizen", "error"), citizen_status)

    #

    report(("citizen", "no-error"), citizen_status)

    #

    pmf_report = Pmf()

    for (cstate, vstate), p in mix.Items():
        creport = report(cstate, citizen_status)
        vreport = report(vstate, vote_status)
        pmf_report[creport, vreport] += p

    #

    pmf_report.Print()

    #
