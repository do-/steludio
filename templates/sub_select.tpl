
	sql (
	
		add_vocabularies ({},
			users => {},
		),
		
		__TYPE__ => [
	
			'id_user',
			
			['label LIKE %?%' => $_REQUEST {q}],
			
			[ LIMIT => 'start, 50'],
		
		],
			
		'users' # , other joined tables
		
	);
	