	my $q = '%' . $_REQUEST {q} . '%';

	my $start = $_REQUEST {start} + 0;

	my ($__TYPE__, $cnt)= sql_select_all_cnt (<<EOS, $q, {fake => '__TYPE__'});
		SELECT
			__TYPE__.*
#			, ???.label AS ???_label
#			, ...
		FROM
			__TYPE__
#			INNER JOIN ??? ON ???.id_??? = ???.id
#			LEFT JOIN ...
		WHERE
			(__TYPE__.label LIKE ?)
		ORDER BY
			__TYPE__.label
		LIMIT
			$start, $$conf{portion}
EOS

	return {
		__TYPE__ => $__TYPE__,
		cnt => $cnt,
		portion => $$conf{portion},
	};