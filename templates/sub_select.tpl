	
	my $item = {
		portion => $conf -> {portion},
	};
	
	my $filter = '';
	my @params = ();
	
	if ($_REQUEST {q}) {		
		$filter .= ' AND __TYPE__.label LIKE ?';
		push @params, '%' . $_REQUEST {q} . '%';		
	}

	if ($_REQUEST {id_user}) {		
		$filter .= ' AND __TYPE__.label = ?';
		push @params, $_REQUEST {id_user};
	}

	my $start = $_REQUEST {start} + 0;

	($item -> {__TYPE__}, $item -> {cnt}) = sql_select_all_cnt (<<EOS, @params, {fake => '__TYPE__'});
		SELECT
			__TYPE__.*
#			, ???.label AS ???_label
#			, ...
		FROM
			__TYPE__
#			INNER JOIN ??? ON ???.id_??? = ???.id
#			LEFT JOIN ...
		WHERE
			1=1
			$filter
		ORDER BY
			__TYPE__.label
		LIMIT
			$start, $$item{portion}
EOS

	return $item;
