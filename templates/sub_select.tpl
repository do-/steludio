
	sql (
	
		add_vocabularies ({},
			users => {},
		),
		
		__TYPE__ => [
	
			'id_user',
			
			['label LIKE %?%' => $_REQUEST {q}],
			
			[ LIMIT => [0 + $_REQUEST {start}, $conf -> {portion}]],
		
		],
			
		'users' # , other joined tables
		
	);
	