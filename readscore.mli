module type READSCORE = 
sig	
	exception NonValidFile
	val lire_partition:string->int list		
end;;