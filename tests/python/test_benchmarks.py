import importlib.util
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]


def module(name):
    spec = importlib.util.spec_from_file_location(name, ROOT/'scripts'/f'{name}.py')
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


class BenchmarkTests(unittest.TestCase):
    def test_pairs_by_seed_not_row_order(self):
        summary = module('summarize_benchmarks')
        actual = [{'seed': '2', 'score': '20'}, {'seed': '1', 'score': '10'}]
        reference = [{'seed': '1', 'score': '5'}, {'seed': '2', 'score': '15'}]
        self.assertEqual(summary.paired_scores(actual, reference), (2, 5, 5, 5))
        with self.assertRaises(ValueError):
            summary.paired_scores(actual+[actual[0]], reference)

    def test_outcome_and_time_units(self):
        benchmark = module('benchmark')
        rows = [dict(score='1000', died='false', gold_collected='true', primitive_actions='1',
                     arrows_fired='0', wumpus_kills='0', planning_nanos='1000000000'),
                dict(score='-1000', died='true', gold_collected='false', primitive_actions='3',
                     arrows_fired='1', wumpus_kills='1', planning_nanos='2000000000')]
        result = benchmark.summary(rows)
        self.assertEqual(result['mean_score'], 0)
        self.assertEqual(result['death_rate'], .5)
        self.assertEqual(result['gold_rate'], .5)
        self.assertEqual(result['planning_seconds'], 3)
        self.assertEqual(result['mean_primitive_actions'], 2)
        self.assertEqual(result['arrows_fired'], 1)
        self.assertEqual(result['wumpus_kill_rate'], .5)
