	
	$item = array (
		portion => $conf [portion],
	);
	
	$filter = '';
	$params = array ();
	
	if ($_REQUEST [q]) {		
		$filter .= ' AND __TYPE__.label LIKE ?';
		array_push ($params, '%' . $_REQUEST [q] . '%');
	}

	$start = $_REQUEST [start] + 0;

	($item [__TYPE__], $item [cnt]) = sql_select_all_cnt (<<<EOS
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
			$start, $item[portion]
EOS
	, $params, {fake => '__TYPE__'});

	return $item;