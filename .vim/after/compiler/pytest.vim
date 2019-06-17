" CompilerSet makeprg=pytest\ --tb=line\ --quiet
CompilerSet makeprg=pytest

"======================================== test session starts ========================================
"platform darwin -- Python 3.7.1, pytest-4.4.1, py-1.8.0, pluggy-0.9.0
"benchmark: 3.2.2 (defaults: timer=time.perf_counter disable_gc=False min_rounds=5 min_time=0.000005 max_time=1.0 calibration_precision=10 warmup=False warmup_iterations=100000)
"rootdir: /Users/por085/src/eme-monorepo/packages/api-comparator
"plugins: parallel-0.0.9, mock-1.10.3, json-0.4.0, cov-2.6.1, benchmark-3.2.2
"collected 2 items

"tests/unit/test_comparator.py F.                                                              [100%]

"============================================= FAILURES ==============================================
"__________________________________ test_compare_when_no_usage_data __________________________________

"period_from = datetime.datetime(2007, 3, 5, 0, 0), period_to = datetime.datetime(2007, 8, 4, 0, 0)
"benchmark_dict = {'benchmark_usage_type': 'with_heating', 'climate_zone': '2', 'fuel_type': 'E', 'household_size': 'large', ...}
"plans = [<comparator.plan.Plan object at 0x109b78278>, <comparator.plan.Plan object at 0x109b784a8>]

"    def test_compare_when_no_usage_data(
"        period_from, period_to, benchmark_dict, plans
"    ):  # noqa
"        """Verify the correct plan is returned when no usage data is available."""
"        comparator = Comparator(benchmark_dict, plans)
"        best = comparator.compare(period_from, period_to)

"        # assert best.annual_cost_without_discounts == 123.45
">       assert False
"E       assert False

"tests/unit/test_comparator.py:37: AssertionError
"================================ 1 failed, 1 passed in 0.10 seconds =================================

CompilerSet errorformat=%f:%l:\ %m
CompilerSet errorformat+=%-G%.%#


" CompilerSet errorformat=
"             \%-G%.%#%.%#%%%.%#,
"             \%-G%.%#FAILURES%.%#,
"             \%f:%l:\ %m,
"             \%-G%.%#in\ %.%#\ seconds,
