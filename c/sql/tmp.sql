
-- SELECTS FIRST LEVEL NODES in the FFO-01 position solved by es
SELECT call_id, player AS pl, alpha, beta, call_level AS cl, empty_count AS ec, legal_move_count AS lmc, legal_move_count_adjusted AS lmca, parent_move AS p_move, t_call_cnt, t_call_cnt - call_id AS scn, t_alpha, t_best_move AS b_move, t_searched_move_cnt AS t_smc, legal_move_array, t_searched_move_array FROM game_tree_log WHERE run_id = 7 AND (call_id = 1 OR parent_hash IN (SELECT hash FROM game_tree_log WHERE run_id = 7 AND call_id = 1)) ORDER BY call_id ASC;
