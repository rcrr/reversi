WITH empty_mobility_node_count AS (
  SELECT
    facts.empty_count                AS empty,
    facts.legal_move_count_adjusted  AS mobility,
    count(*)                         AS node_count
  FROM
    rand_log AS facts
  GROUP BY
    empty, mobility
  ORDER BY
    empty DESC, mobility ASC
), empty_sum AS (
  SELECT
    a.empty           AS empty,
    sum(a.node_count) AS sum
  FROM
    empty_mobility_node_count AS a
  GROUP BY
    empty
), empty_mobility_frequency AS (
  SELECT
    b.empty               AS empty,
    b.mobility            AS mobility,
    b.node_count          AS node_count,
    b.node_count / c.sum  AS frequency
  FROM
    empty_mobility_node_count   AS b
  INNER JOIN
    empty_sum                   AS c
  ON (b.empty = c.empty)
), empty_average_mobility AS (
  SELECT
    d.empty                         AS empty,
    sum(d.frequency * d.mobility)   AS average_mobility
  FROM
    empty_mobility_frequency AS d
  GROUP BY
    empty
), empty_mobility_variance AS (
  SELECT
    e.empty               AS empty,
    e.mobility            AS mobility,
    e.frequency           AS frequency,
    f.average_mobility    AS average_mobility
  FROM
    empty_mobility_frequency AS e
  INNER JOIN
    empty_average_mobility   AS f
  ON (e.empty = f.empty)
)
SELECT
  t.empty                                                                 AS empty,
  round(max(t.average_mobility), 3)                                       AS average_mobility,
  round(sum((t.average_mobility - t.mobility)^2 * t.frequency), 4)        AS mobility_variance,
  round(sqrt(sum((t.average_mobility - t.mobility)^2 * t.frequency)), 4)  AS mobility_sd
FROM
  empty_mobility_variance AS t
GROUP BY
  empty
ORDER BY
  empty DESC;