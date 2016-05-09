package modules;

import com.google.inject.AbstractModule;
import redis.clients.jedis.Jedis;
import util.LogUtil;
import util.RedisPool;


/**
 * 启动leveldb
 * Created by howen on 16/2/19.
 */
public class LevelDBModule extends AbstractModule {

    protected void configure() {
        bind(LogUtil.class).asEagerSingleton();
        bind(RedisPool.class).asEagerSingleton();
        bind(Jedis.class).toInstance(RedisPool.create());
    }
}
