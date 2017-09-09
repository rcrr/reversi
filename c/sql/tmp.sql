
SELECT count(*) FROM ( SELECT (gtl.json_doc->'cl')::TEXT::INT AS jcl, gtl.call_level AS cl FROM game_tree_log AS gtl WHERE run_id IN (1, 2, 3, 4, 5, 6, 7, 8)) AS TEST WHERE jcl != cl;

SELECT count(*) FROM ( SELECT (gtl.json_doc->'ec')::TEXT::INT AS jec, gtl.empty_count AS ec FROM game_tree_log AS gtl WHERE run_id IN (1, 2, 3, 4, 5, 6, 7, 8)) AS TEST WHERE jec != ec;

SELECT count(*) FROM ( SELECT (gtl.json_doc->'il')::TEXT::BOOLEAN AS jil, gtl.is_leaf AS il FROM game_tree_log AS gtl WHERE run_id IN (1, 2, 3, 4, 5, 6, 7, 8)) AS TEST WHERE jil != il;

SELECT count(*) FROM ( SELECT (gtl.json_doc->'lmc')::TEXT::INT AS jlmc, gtl.legal_move_count AS lmc FROM game_tree_log AS gtl WHERE run_id IN (1, 2, 3, 4, 5, 6, 7, 8)) AS TEST WHERE jlmc != lmc;

SELECT count(*) FROM ( SELECT (gtl.json_doc->'lmca')::TEXT::INT AS jlmca, gtl.legal_move_count_adjusted AS lmca FROM game_tree_log AS gtl WHERE run_id IN (1, 2, 3, 4, 5, 6, 7, 8)) AS TEST WHERE jlmca != lmca;

SELECT count(*) FROM ( SELECT replace(replace(gtl.json_doc->>'lma', '[', '{'), ']', '}')::square[] AS jlma, gtl.legal_move_array AS lma FROM game_tree_log AS gtl WHERE run_id IN (1, 2, 3, 4, 5, 6, 7, 8)) AS TEST WHERE jlma != lma;
