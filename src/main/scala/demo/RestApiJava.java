package demo;

import java.util.HashMap;
import java.util.NoSuchElementException;

public class RestApiJava {

    private HashMap<String,Integer> state = new HashMap<>();

    public synchronized Integer GET(String key){
        Integer value = state.get(key);
        if(value == null) throw new NoSuchElementException("bang");
        return value;
    }

    public synchronized String PUT(String key, Integer value){
        state.put(key,value);
        return key;
    }

    public synchronized boolean DELETE(String key){
        boolean found = state.containsKey(key);
        //state.remove(key);
        return found;
    }

}
